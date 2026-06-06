"""
Roth IRA Stock Picker — Flask server
Run:  python app.py
Open: http://localhost:5000
"""
from flask import Flask, render_template, jsonify, request
from stock_picker import get_monthly_picks, get_chart_data
import json, os, uuid

app = Flask(__name__)
PORTFOLIO_FILE = "portfolio.json"


def _load_pf() -> dict:
    if os.path.exists(PORTFOLIO_FILE):
        with open(PORTFOLIO_FILE) as f:
            return json.load(f)
    return {"positions": []}


def _save_pf(data: dict) -> None:
    with open(PORTFOLIO_FILE, "w") as f:
        json.dump(data, f, indent=2)


@app.get("/")
def index():
    return render_template("index.html")


@app.get("/api/picks")
def picks():
    force = request.args.get("force") == "1"
    return jsonify(get_monthly_picks(force=force))


@app.get("/api/chart/<ticker>")
def chart(ticker: str):
    period = request.args.get("period", "3mo")
    return jsonify(get_chart_data(ticker.upper(), period))


@app.get("/api/portfolio")
def portfolio_get():
    return jsonify(_load_pf())


@app.post("/api/portfolio")
def portfolio_add():
    pf = _load_pf()
    pos = request.json
    pos["id"] = str(uuid.uuid4())
    pf["positions"].append(pos)
    _save_pf(pf)
    return jsonify({"ok": True, "id": pos["id"]})


@app.delete("/api/portfolio/<pos_id>")
def portfolio_delete(pos_id: str):
    pf = _load_pf()
    pf["positions"] = [p for p in pf["positions"] if p.get("id") != pos_id]
    _save_pf(pf)
    return jsonify({"ok": True})


if __name__ == "__main__":
    print("\n" + "=" * 52)
    print("  🚀  Roth IRA Stock Picker")
    print("=" * 52)
    print("  Browser: http://localhost:5000")
    print("  First load takes ~20s (scanning the market)")
    print("  Ctrl+C to stop")
    print("=" * 52 + "\n")
    app.run(debug=False, port=5000, host="0.0.0.0")
