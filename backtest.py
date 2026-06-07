"""
Monthly rebalancing backtest for the momentum strategy.
Simulates picking top stocks each month and tracking returns over 3 years.

Survivorship bias caveat: the universe is defined today, not at each historical
date. This means we're testing on stocks we already know exist/survived.
Real forward results may differ, but the momentum logic itself is validated.
"""
import yfinance as yf
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import json, os, time

from stock_picker import UNIVERSE

BACKTEST_CACHE = "backtest_cache.json"
CACHE_HOURS    = 24  # expensive to run, cache for a full day


def _score_series(closes: pd.Series) -> float | None:
    """
    Same momentum scoring logic as the live picker.
    Runs on a historical price slice with no lookahead.
    """
    if len(closes) < 60:
        return None

    c     = closes.copy()
    price = float(c.iloc[-1])

    def pct(days: int) -> float:
        idx = min(days, len(c) - 1)
        return float((c.iloc[-1] / c.iloc[-idx] - 1) * 100)

    r1m, r3m, r6m = pct(21), pct(63), pct(126)

    # Momentum (55%)
    mom = (
        0.50 * float(np.clip(r1m / 25 * 50 + 50, 0, 100)) +
        0.30 * float(np.clip(r3m / 50 * 50 + 50, 0, 100)) +
        0.20 * float(np.clip(r6m / 80 * 50 + 50, 0, 100))
    )

    # Trend (28%)
    ma20  = float(c.rolling(20).mean().iloc[-1])
    ma50  = float(c.rolling(50).mean().iloc[-1])
    ma200 = float(c.rolling(200).mean().iloc[-1]) if len(c) >= 200 else ma50 * 0.92
    trend = (
        int(price > ma20)  * 20 +
        int(price > ma50)  * 30 +
        int(price > ma200) * 20 +
        int(ma50  > ma200) * 30
    )

    # RSI (17%) — no volume data in bulk download so we drop that component
    delta = c.diff().dropna()
    gain  = delta.clip(lower=0).rolling(14).mean()
    loss  = (-delta.clip(upper=0)).rolling(14).mean()
    with np.errstate(divide="ignore", invalid="ignore"):
        rs = gain / loss
    rsi_s = (100 - 100 / (1 + rs)).dropna()
    rsi   = float(rsi_s.iloc[-1]) if not rsi_s.empty else 50.0

    if 45 <= rsi <= 72:
        rsi_score = 100.0
    elif rsi < 45:
        rsi_score = (rsi / 45) * 100
    else:
        rsi_score = max(0.0, (100 - rsi) / 28 * 100)

    return mom * 0.55 + trend * 0.28 + rsi_score * 0.17


def _diversify(scored: list[dict], top_n: int) -> list[dict]:
    """Pick top N with at least one from each category."""
    ranked = sorted(scored, key=lambda x: x["score"], reverse=True)
    picks, seen = [], set()
    for s in ranked:
        if s["category"] not in seen and len(picks) < top_n:
            picks.append(s)
            seen.add(s["category"])
    for s in ranked:
        if s not in picks and len(picks) < top_n:
            picks.append(s)
    return picks[:top_n]


def run_backtest(lookback_years: int = 3, top_n: int = 6, force: bool = False) -> dict:
    # ── Cache ──────────────────────────────────────────────────────────────────
    if not force and os.path.exists(BACKTEST_CACHE):
        with open(BACKTEST_CACHE) as f:
            cached = json.load(f)
        age_h = (time.time() - cached.get("ts", 0)) / 3600
        if age_h < CACHE_HOURS:
            cached["from_cache"] = True
            return cached

    all_tickers   = [t for tks in UNIVERSE.values() for t in tks]
    ticker_to_cat = {t: cat for cat, tks in UNIVERSE.items() for t in tks}
    to_download   = all_tickers + ["SPY", "QQQ"]

    end_date   = datetime.now()
    start_date = end_date - timedelta(days=lookback_years * 365 + 250)  # 250d MA warmup

    print(f"Downloading {len(to_download)} tickers for {lookback_years}-year backtest…")

    try:
        raw = yf.download(
            to_download,
            start=start_date.strftime("%Y-%m-%d"),
            end=end_date.strftime("%Y-%m-%d"),
            auto_adjust=True,
            progress=False,
            threads=True,
        )
    except Exception as e:
        return {"error": f"Data download failed: {e}", "monthly": [], "equity_curve": [], "metrics": {}}

    # ── Extract close prices (handles yfinance MultiIndex format) ──────────────
    if not isinstance(raw.columns, pd.MultiIndex):
        return {"error": "Unexpected yfinance response format.", "monthly": [], "equity_curve": [], "metrics": {}}

    try:
        closes_df = raw["Close"]
    except KeyError:
        return {"error": "Could not extract Close prices.", "monthly": [], "equity_curve": [], "metrics": {}}

    spy = closes_df["SPY"].dropna() if "SPY" in closes_df.columns else pd.Series(dtype=float)
    qqq = closes_df["QQQ"].dropna() if "QQQ" in closes_df.columns else spy

    if spy.empty:
        return {"error": "SPY data unavailable.", "monthly": [], "equity_curve": [], "metrics": {}}

    # ── Monthly rebalance dates ────────────────────────────────────────────────
    warmup_end = start_date + timedelta(days=230)
    try:
        month_ends = pd.date_range(
            start=warmup_end,
            end=end_date - timedelta(days=20),
            freq="ME",
        )
    except Exception:
        month_ends = pd.date_range(
            start=warmup_end,
            end=end_date - timedelta(days=20),
            freq="M",
        )

    if len(month_ends) < 6:
        return {"error": "Not enough historical data.", "monthly": [], "equity_curve": [], "metrics": {}}

    # ── Month-by-month simulation ──────────────────────────────────────────────
    portfolio_val = 1.0
    spy_val       = 1.0
    qqq_val       = 1.0
    peak_val      = 1.0

    monthly    = []
    eq_curve   = [{"date": month_ends[0].strftime("%Y-%m"), "port": 1.0, "spy": 1.0, "qqq": 1.0}]
    drawdowns  = []

    for i in range(len(month_ends) - 1):
        buy_dt  = month_ends[i]
        sell_dt = month_ends[i + 1]

        # Score every ticker using only data available on buy_dt (no lookahead bias)
        scored = []
        for ticker in all_tickers:
            if ticker not in closes_df.columns:
                continue
            hist = closes_df[ticker].loc[:buy_dt].dropna()
            s    = _score_series(hist)
            if s is not None:
                scored.append({"ticker": ticker, "category": ticker_to_cat[ticker], "score": s})

        if not scored:
            continue

        picks  = _diversify(scored, top_n)
        scores = np.array([p["score"] for p in picks], dtype=float)
        weights = scores ** 2
        weights /= weights.sum()

        # Compute weighted portfolio return for this month
        port_ret     = 0.0
        weight_used  = 0.0
        pick_details = []

        for p, w in zip(picks, weights):
            col = closes_df.get(p["ticker"]) if hasattr(closes_df, "get") else (
                closes_df[p["ticker"]] if p["ticker"] in closes_df.columns else None
            )
            if col is None:
                continue
            after_buy  = col[col.index >= buy_dt].dropna()
            after_sell = col[col.index >= sell_dt].dropna()
            if after_buy.empty or after_sell.empty:
                continue
            p_buy  = float(after_buy.iloc[0])
            p_sell = float(after_sell.iloc[0])
            if p_buy == 0 or np.isnan(p_buy) or np.isnan(p_sell):
                continue
            ret = (p_sell / p_buy) - 1
            port_ret    += ret * w
            weight_used += w
            pick_details.append({
                "ticker": p["ticker"],
                "ret":    round(ret * 100, 1),
                "w":      round(float(w) * 100, 1),
            })

        if weight_used < 0.2:
            continue
        if weight_used < 1.0:
            port_ret /= weight_used  # normalise for any missing picks

        # Benchmark returns for same period
        def bench_ret(prices: pd.Series, d1, d2) -> float:
            a = prices[prices.index >= d1].dropna()
            b = prices[prices.index >= d2].dropna()
            if a.empty or b.empty:
                return 0.0
            v1, v2 = float(a.iloc[0]), float(b.iloc[0])
            return 0.0 if v1 == 0 or np.isnan(v1) or np.isnan(v2) else (v2 / v1) - 1

        spy_ret = bench_ret(spy, buy_dt, sell_dt)
        qqq_ret = bench_ret(qqq, buy_dt, sell_dt)

        portfolio_val *= (1 + port_ret)
        spy_val       *= (1 + spy_ret)
        qqq_val       *= (1 + qqq_ret)
        peak_val       = max(peak_val, portfolio_val)
        drawdowns.append((portfolio_val / peak_val - 1) * 100)

        monthly.append({
            "date":     sell_dt.strftime("%Y-%m"),
            "port_ret": round(port_ret * 100, 2),
            "spy_ret":  round(spy_ret * 100, 2),
            "qqq_ret":  round(qqq_ret * 100, 2),
            "port_val": round(portfolio_val, 4),
            "picks":    pick_details,
        })
        eq_curve.append({
            "date": sell_dt.strftime("%Y-%m"),
            "port": round(portfolio_val, 4),
            "spy":  round(spy_val, 4),
            "qqq":  round(qqq_val, 4),
        })

    if not monthly:
        return {"error": "Simulation produced no results.", "monthly": [], "equity_curve": [], "metrics": {}}

    # ── Summary metrics ────────────────────────────────────────────────────────
    port_rets = [m["port_ret"] for m in monthly]
    n         = len(monthly)
    ann       = 12 / n

    std_m   = float(np.std(port_rets)) if len(port_rets) > 1 else 1.0
    sharpe  = float(np.mean(port_rets)) / std_m * np.sqrt(12) if std_m > 0 else 0.0

    metrics = {
        "total_ret":   round((portfolio_val - 1) * 100, 1),
        "spy_total":   round((spy_val - 1) * 100, 1),
        "qqq_total":   round((qqq_val - 1) * 100, 1),
        "ann_ret":     round(((portfolio_val ** ann) - 1) * 100, 1),
        "spy_ann":     round(((spy_val ** ann) - 1) * 100, 1),
        "qqq_ann":     round(((qqq_val ** ann) - 1) * 100, 1),
        "sharpe":      round(sharpe, 2),
        "win_rate":    round(sum(1 for r in port_rets if r > 0) / n * 100, 1),
        "max_dd":      round(min(drawdowns), 1) if drawdowns else 0.0,
        "avg_monthly": round(float(np.mean(port_rets)), 2),
        "n_months":    n,
        "final_700":   round(700 * portfolio_val, 2),
    }

    out = {
        "monthly":        monthly,
        "equity_curve":   eq_curve,
        "metrics":        metrics,
        "lookback_years": lookback_years,
        "generated_at":   datetime.now().strftime("%b %d, %Y %H:%M"),
        "ts":             time.time(),
        "from_cache":     False,
    }
    with open(BACKTEST_CACHE, "w") as f:
        json.dump(out, f, indent=2)
    return out
