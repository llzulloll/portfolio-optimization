"""
Aggressive momentum stock picker for a Roth IRA.
Scores stocks on price momentum, trend signals, RSI, and volume surge.
Uses Yahoo Finance — no API key needed.
"""
import yfinance as yf
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
from concurrent.futures import ThreadPoolExecutor, as_completed
import json, os, time

CACHE_FILE = "picks_cache.json"
CACHE_HOURS = 6  # auto-refresh every 6h; force anytime via the button

# ─── Stock universe ──────────────────────────────────────────────────────────
# Aggressive picks for a 21-year-old: high-growth tech, small-cap rockets,
# and leveraged ETFs for maximum upside exposure.
UNIVERSE: dict[str, list[str]] = {
    "AI & High-Growth Tech": [
        "NVDA", "AMD", "META", "AVGO", "MSFT", "GOOGL", "ARM",
        "CRWD", "NET", "PLTR", "AXON", "APP", "DDOG", "TTD",
        "DUOL", "CAVA", "CELH", "SMCI",
    ],
    "Small-Cap Rockets": [
        "RKLB", "ASTS", "ACHR", "JOBY", "LUNR", "SOUN",
        "IONQ", "RXRX", "APLD", "HOOD", "SOFI", "UPST",
        "BBAI", "OKLO", "NNE",
    ],
    "Leveraged ETFs": [
        "TQQQ", "SOXL", "TECL", "UPRO", "FNGU",
    ],
}


def _rsi(prices: pd.Series, period: int = 14) -> float:
    delta = prices.diff().dropna()
    gain = delta.clip(lower=0).rolling(period).mean()
    loss = (-delta.clip(upper=0)).rolling(period).mean()
    with np.errstate(divide="ignore", invalid="ignore"):
        rs = gain / loss
    series = 100 - (100 / (1 + rs))
    return float(series.dropna().iloc[-1]) if not series.dropna().empty else 50.0


def _score_ticker(ticker: str, category: str) -> dict | None:
    try:
        hist = yf.Ticker(ticker).history(period="1y", timeout=15)
        if len(hist) < 60:
            return None

        c = hist["Close"].dropna()
        v = hist["Volume"].dropna()
        price = float(c.iloc[-1])

        def pct(days: int) -> float:
            idx = min(days, len(c) - 1)
            return float((c.iloc[-1] / c.iloc[-idx] - 1) * 100)

        r1m, r3m, r6m = pct(21), pct(63), pct(126)

        # ── Momentum score (50%) — weighted combo of 1M/3M/6M returns ──────
        mom = (
            0.50 * float(np.clip(r1m / 25 * 50 + 50, 0, 100)) +
            0.30 * float(np.clip(r3m / 50 * 50 + 50, 0, 100)) +
            0.20 * float(np.clip(r6m / 80 * 50 + 50, 0, 100))
        )

        # ── Trend score (25%) — moving average signals ───────────────────
        ma20  = float(c.rolling(20).mean().iloc[-1])
        ma50  = float(c.rolling(50).mean().iloc[-1])
        ma200 = float(c.rolling(200).mean().iloc[-1]) if len(c) >= 200 else ma50 * 0.92

        trend = (
            int(price > ma20)  * 20 +
            int(price > ma50)  * 30 +
            int(price > ma200) * 20 +
            int(ma50  > ma200) * 30
        )

        # ── RSI score (15%) — reward healthy momentum, penalize extremes ─
        rsi = _rsi(c)
        if 45 <= rsi <= 72:
            rsi_score = 100.0
        elif rsi < 45:
            rsi_score = (rsi / 45) * 100
        else:
            rsi_score = max(0.0, (100 - rsi) / 28 * 100)

        # ── Volume score (10%) — rising volume = smart money buying ──────
        vol_now  = float(v.iloc[-10:].mean())
        vol_prev = float(v.iloc[-40:-10].mean())
        vol_ratio = (vol_now / vol_prev) if vol_prev > 0 else 1.0
        vol_score = float(np.clip((vol_ratio - 0.5) / 1.5 * 100, 0, 100))

        final = mom * 0.50 + trend * 0.25 + rsi_score * 0.15 + vol_score * 0.10

        # ── Human-readable signals for the UI ────────────────────────────
        signals: list[dict] = []
        if r1m > 15:
            signals.append({"text": f"+{r1m:.0f}% this month", "cls": "success"})
        elif r1m < -10:
            signals.append({"text": f"{r1m:.0f}% this month",  "cls": "danger"})
        if ma50 > ma200:
            signals.append({"text": "Golden Cross",             "cls": "success"})
        if price > ma50:
            signals.append({"text": "Above 50-day MA",          "cls": "success"})
        else:
            signals.append({"text": "Below 50-day MA",          "cls": "warning"})
        if vol_ratio > 1.4:
            signals.append({"text": "Volume Surge",             "cls": "info"})
        if rsi > 72:
            signals.append({"text": f"RSI {rsi:.0f} — hot",    "cls": "warning"})
        elif 50 <= rsi <= 72:
            signals.append({"text": f"RSI {rsi:.0f}",          "cls": "success"})

        # Plain text reason for "Why this pick?"
        reasons = []
        if r1m > 15:
            reasons.append(f"up {r1m:.0f}% this month")
        if r3m > 25:
            reasons.append(f"up {r3m:.0f}% over 3 months")
        if ma50 > ma200:
            reasons.append("golden cross (50MA above 200MA)")
        if vol_ratio > 1.3:
            reasons.append("volume surge (institutional buying)")
        if price > ma50:
            reasons.append("trading above its 50-day moving average")
        why = ("Strong momentum: " + ", ".join(reasons) + ".") if reasons else "High momentum score."

        return {
            "ticker":    ticker,
            "category":  category,
            "price":     round(price, 2),
            "score":     round(final, 1),
            "r1m":       round(r1m, 1),
            "r3m":       round(r3m, 1),
            "r6m":       round(r6m, 1),
            "rsi":       round(rsi, 1),
            "above50":   bool(price > ma50),
            "golden":    bool(ma50 > ma200),
            "vol_ratio": round(vol_ratio, 2),
            "signals":   signals,
            "why":       why,
            "leveraged": category == "Leveraged ETFs",
        }
    except Exception as e:
        print(f"  [skip] {ticker}: {e}")
        return None


def _allocate(picks: list[dict], budget: float) -> list[dict]:
    scores = np.array([p["score"] for p in picks], dtype=float)
    weights = scores ** 2  # square-weight to concentrate money in top picks
    weights /= weights.sum()
    for p, w in zip(picks, weights):
        p["alloc_pct"] = round(float(w * 100), 1)
        p["alloc_usd"] = round(float(w * budget), 2)
    return picks


def get_monthly_picks(force: bool = False, budget: float = 700, top_n: int = 6) -> dict:
    # Serve from cache if fresh enough
    if not force and os.path.exists(CACHE_FILE):
        with open(CACHE_FILE) as f:
            cached = json.load(f)
        age_h = (time.time() - cached.get("ts", 0)) / 3600
        if age_h < CACHE_HOURS:
            cached["from_cache"] = True
            cached["cache_age_h"] = round(age_h, 1)
            return cached

    print("Scanning market (this takes ~20 seconds)...")
    all_args = [(t, cat) for cat, tks in UNIVERSE.items() for t in tks]

    results: list[dict] = []
    with ThreadPoolExecutor(max_workers=10) as ex:
        futures = {ex.submit(_score_ticker, t, cat): t for t, cat in all_args}
        for fut in as_completed(futures):
            res = fut.result()
            if res:
                results.append(res)

    if not results:
        return {"error": "Could not fetch market data. Check your internet connection.", "picks": []}

    results.sort(key=lambda x: x["score"], reverse=True)

    # Ensure at least 1 pick from each category, then fill by score
    picks: list[dict] = []
    seen_cats: set[str] = set()
    for r in results:
        if r["category"] not in seen_cats and len(picks) < top_n:
            picks.append(r)
            seen_cats.add(r["category"])
    for r in results:
        if r not in picks and len(picks) < top_n:
            picks.append(r)
    picks.sort(key=lambda x: x["score"], reverse=True)

    picks = _allocate(picks, budget)

    out = {
        "picks":        picks,
        "generated_at": datetime.now().strftime("%b %d, %Y %H:%M"),
        "ts":           time.time(),
        "budget":       budget,
        "from_cache":   False,
        "total_scored": len(results),
        "next_refresh": (datetime.now() + timedelta(days=30)).strftime("%B %d, %Y"),
        "all_scores":   sorted(results, key=lambda x: x["score"], reverse=True)[:20],
    }
    with open(CACHE_FILE, "w") as f:
        json.dump(out, f, indent=2)
    return out


def get_chart_data(ticker: str, period: str = "3mo") -> dict:
    try:
        hist = yf.Ticker(ticker).history(period=period)
        closes = hist["Close"].dropna()
        # Calculate 20-day MA for chart overlay
        ma20 = closes.rolling(20).mean()
        return {
            "dates":  [d.strftime("%m/%d") for d in closes.index],
            "prices": [round(float(p), 2) for p in closes],
            "ma20":   [round(float(p), 2) if not np.isnan(p) else None for p in ma20],
        }
    except Exception as e:
        print(f"Chart error {ticker}: {e}")
        return {"dates": [], "prices": [], "ma20": []}
