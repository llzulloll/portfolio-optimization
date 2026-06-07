"""
Roth IRA Stock Picker — Flask server
Local:  python app.py  →  http://localhost:5000
Deploy: Render.com (see README for steps)
"""
from flask import Flask, render_template, jsonify, request
from flask_sqlalchemy import SQLAlchemy
from stock_picker import get_monthly_picks, get_chart_data
from backtest import run_backtest
import os, uuid

app = Flask(__name__)

# SQLite locally, upgradeable to Postgres via DATABASE_URL env var on Render
db_url = os.environ.get("DATABASE_URL", "sqlite:///portfolio.db")
# Render gives postgres:// but SQLAlchemy needs postgresql://
if db_url.startswith("postgres://"):
    db_url = db_url.replace("postgres://", "postgresql://", 1)

app.config["SQLALCHEMY_DATABASE_URI"] = db_url
app.config["SQLALCHEMY_TRACK_MODIFICATIONS"] = False
db = SQLAlchemy(app)


class Position(db.Model):
    id        = db.Column(db.String(36), primary_key=True, default=lambda: str(uuid.uuid4()))
    ticker    = db.Column(db.String(10),  nullable=False)
    shares    = db.Column(db.Float,       nullable=False)
    avg_cost  = db.Column(db.Float,       nullable=False)
    date      = db.Column(db.String(20),  nullable=True)

    def to_dict(self):
        return {
            "id":       self.id,
            "ticker":   self.ticker,
            "shares":   self.shares,
            "avg_cost": self.avg_cost,
            "date":     self.date,
        }


with app.app_context():
    db.create_all()


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


@app.get("/api/backtest")
def backtest():
    force = request.args.get("force") == "1"
    years = int(request.args.get("years", "3"))
    return jsonify(run_backtest(lookback_years=years, force=force))


@app.get("/api/portfolio")
def portfolio_get():
    positions = Position.query.all()
    return jsonify({"positions": [p.to_dict() for p in positions]})


@app.post("/api/portfolio")
def portfolio_add():
    data = request.json
    pos  = Position(
        ticker   = data["ticker"].upper(),
        shares   = float(data["shares"]),
        avg_cost = float(data["avg_cost"]),
        date     = data.get("date"),
    )
    db.session.add(pos)
    db.session.commit()
    return jsonify({"ok": True, "id": pos.id})


@app.delete("/api/portfolio/<pos_id>")
def portfolio_delete(pos_id: str):
    pos = Position.query.get(pos_id)
    if pos:
        db.session.delete(pos)
        db.session.commit()
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
