"""
SHERLOCK EXPERT SYSTEM - CORRECTED & STABLE VERSION
Flask + PySwip + Prolog
"""

from flask import Flask, request, jsonify, send_from_directory
from flask_cors import CORS
from pyswip import Prolog
import os

# --------------------------------------------------
# Flask setup
# --------------------------------------------------
app = Flask(__name__)
CORS(app)

# --------------------------------------------------
# Prolog setup
# --------------------------------------------------
prolog = Prolog()

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROLOG_FILE = os.path.join(BASE_DIR, "sherlockk.pl")

if not os.path.exists(PROLOG_FILE):
    raise FileNotFoundError(f"sherlockk.pl not found in {BASE_DIR}")

print(f"✅ Loading Prolog file: {PROLOG_FILE}")
prolog.consult(PROLOG_FILE)
print("✅ Prolog engine loaded")

# --------------------------------------------------
# Helpers
# --------------------------------------------------
def debug_prolog():
    print("\n====== PROLOG MEMORY ======")
    list(prolog.query("listing."))
    print("===========================\n")

# --------------------------------------------------
# Routes
# --------------------------------------------------

@app.route("/")
def home():
    return send_from_directory(".", "index.html")


# ---------------- TEST PROLOG ----------------------

@app.route("/test-prolog", methods=["GET"])
def test_prolog():
    try:
        res = list(prolog.query("attribut_valide(genre, X)"))
        return jsonify({
            "success": True,
            "message": "Prolog is working",
            "sample": [r["X"] for r in res]
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


# ---------------- DEBUG MEMORY --------------------

@app.route("/whats-in-prolog", methods=["GET"])
def whats_in_prolog():
    try:
        suspects = [s["X"] for s in prolog.query("suspect(X)")]
        caracs = [
            f"{c['S']} : {c['C']} = {c['V']}"
            for c in prolog.query("caracteristique(S,C,V)")
        ]
        indices = [
            f"{i['I']} = {i['V']}"
            for i in prolog.query("indice(I,V)")
        ]

        return jsonify({
            "success": True,
            "suspects": suspects,
            "caracteristiques": caracs,
            "indices": indices
        })
    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


# ---------------- RESET ---------------------------

@app.route("/reset", methods=["POST"])
def reset():
    prolog.query("retractall(suspect(_))")
    prolog.query("retractall(caracteristique(_,_,_))")
    prolog.query("retractall(indice(_,_))")
    print("🔴 Prolog reset")
    debug_prolog()
    return jsonify({"success": True})


# ---------------- ADD SUSPECT ---------------------

@app.route("/add-suspect", methods=["POST"])
def add_suspect():
    try:
        data = request.json
        nom = data["nom"]

        print(f"\n➕ Adding suspect: {nom}")
        prolog.assertz(f"suspect('{nom}')")

        for carac, valeur in data.get("caracteristiques", {}).items():
            if valeur not in ("", None, "inconnu"):
                val = str(valeur).lower()
                prolog.assertz(
                    f"caracteristique('{nom}', {carac}, {val})"
                )
                print(f"   - {carac}: {val}")

        debug_prolog()
        return jsonify({"success": True})

    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


# ---------------- REMOVE SUSPECT ------------------

@app.route("/remove-suspect", methods=["POST"])
def remove_suspect():
    try:
        nom = request.json["nom"]
        print(f"\n➖ Removing suspect: {nom}")

        prolog.query(f"retractall(suspect('{nom}'))")
        prolog.query(f"retractall(caracteristique('{nom}',_,_))")

        debug_prolog()
        return jsonify({"success": True})

    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


# ---------------- ADD INDICES ---------------------

@app.route("/add-indices", methods=["POST"])
def add_indices():
    try:
        indices = request.json["indices"]

        print("\n🔎 Adding indices")
        prolog.query("retractall(indice(_,_))")

        for ind, val in indices.items():
            if val not in ("", None, "inconnu"):
                val = str(val).lower()
                prolog.assertz(f"indice({ind}, {val})")
                print(f"   - {ind}: {val}")

        debug_prolog()
        return jsonify({"success": True})

    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


# ---------------- RUN INFERENCE -------------------

@app.route("/run-inference", methods=["POST"])
def run_inference():
    try:
        print("\n🧠 Running inference...")
        results = []

        for sol in prolog.query(
            "coupable_hybride(Suspect, Score, Explication)"
        ):
            results.append({
                "suspect": sol["Suspect"],
                "score": round(float(sol["Score"]), 1),
                "explication": sol["Explication"]
            })

        results.sort(key=lambda x: x["score"], reverse=True)
        return jsonify({"success": True, "results": results})

    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


# ---------------- STATELESS ANALYZE ----------------

@app.route("/analyze", methods=["POST"])
def analyze():
    try:
        data = request.json

        # Reset Prolog (stateless mode)
        prolog.query("retractall(suspect(_))")
        prolog.query("retractall(caracteristique(_,_,_))")
        prolog.query("retractall(indice(_,_))")

        for s in data["suspects"]:
            nom = s["nom"]
            prolog.assertz(f"suspect('{nom}')")

            for c, v in s["caracteristiques"].items():
                if v not in ("", None, "inconnu"):
                    prolog.assertz(
                        f"caracteristique('{nom}', {c}, {str(v).lower()})"
                    )

        for i, v in data["indices"].items():
            if v not in ("", None, "inconnu"):
                prolog.assertz(f"indice({i}, {str(v).lower()})")

        return run_inference()

    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 500


# --------------------------------------------------
# RUN SERVER (MUST BE LAST)
# --------------------------------------------------
if __name__ == "__main__":
    print("\n" + "=" * 50)
    print("🔎 SHERLOCK EXPERT SYSTEM")
    print("🌐 http://localhost:5000")
    print("🧪 /test-prolog")
    print("🧠 /whats-in-prolog")
    print("=" * 50 + "\n")

    app.run(debug=True, port=5000)
