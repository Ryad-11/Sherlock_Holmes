# 🔍 Sherlock Holmes Expert System

> *"When you have eliminated the impossible, whatever remains, however improbable, must be the truth."*

An intelligent criminal investigation system built as part of the **Introduction to Artificial Intelligence** module at École Nationale Polytechnique (ENP). The system takes a set of suspects and crime scene evidence, then uses classical AI techniques to deduce the most probable culprit — ranked by score.

---

## The Module

This project covers four core AI concepts studied in the **Introduction to Artificial Intelligence** course:

| Concept | Role in this project |
|---------|---------------------|
| **Intelligent Agents** | The system acts as a reasoning agent: it perceives evidence (input), reasons over it (inference engine), and produces a ranked verdict (output) |
| **Predicate Logic** | Implemented in Prolog — facts about suspects and crime scenes are represented as logical predicates and queried via resolution |
| **Constraint Satisfaction Problems (CSP)** | Each suspect must satisfy a set of hard constraints (physical match, alibi, weapon compatibility) to remain a valid candidate |
| **Decision Trees** | A rule-based tree classifies each suspect into a suspicion class (très suspect → très peu suspect) based on ordered attribute checks |

These are not used in isolation — the system combines all four into a **hybrid inference engine** that produces a final score for each suspect.

---

## Architecture — The Hybrid Engine

The reasoning pipeline has three layers that run in sequence for each suspect:

```
Evidence + Suspect Profile
           │
           ▼
  ┌─────────────────────────────────────┐
  │  LAYER 1 — CSP FILTER               │
  │                                     │
  │  Hard constraints — any failure     │
  │  eliminates the suspect entirely:   │
  │                                     │
  │  · Alibi verified → eliminated      │
  │  · Physical evidence mismatch       │
  │    (footprint, DNA, blood type,     │
  │     hair color) → eliminated        │
  │  · Weapon incompatibility           │
  │    (heavy weapon needs high force,  │
  │     poison needs medical profession,│
  │     firearm needs legal access)     │
  │    → eliminated                     │
  │  · Motive incoherence → eliminated  │
  │  · Nighttime crime + no nocturnal   │
  │    habit → eliminated               │
  └──────────────┬──────────────────────┘
                 │ Suspects that pass all constraints
                 ▼
  ┌─────────────────────────────────────┐
  │  LAYER 2 — DECISION TREE            │
  │                                     │
  │  Ordered attribute checks:          │
  │                                     │
  │  Alibi verified?                    │
  │    └─ YES → très_peu_suspect (10)   │
  │    └─ NO  → Motive coherent?        │
  │               └─ YES → Force high?  │
  │                          └─ YES →   │
  │                            Evidence │
  │                            match?   │
  │                            YES →    │
  │                            très_    │
  │                            suspect  │
  │                            (90)     │
  │                            NO →     │
  │                            suspect  │
  │                            (70)     │
  │                          └─ NO →    │
  │                            peu_     │
  │                            suspect  │
  │                            (40)     │
  │               └─ NO  → peu_suspect  │
  │                          (40)       │
  └──────────────┬──────────────────────┘
                 │ Score from tree (10 / 40 / 70 / 90)
                 ▼
  ┌─────────────────────────────────────┐
  │  LAYER 3 — BAYESIAN NETWORK         │
  │                                     │
  │  Prior probability: P(guilty) = 0.1 │
  │                                     │
  │  Likelihood ratio multipliers:      │
  │  · Motive coherent        × 3.0     │
  │  · Weak/no alibi          × 4.0     │
  │  · Physical match         × 5.0     │
  │  · Serious criminal record× 1.8     │
  │  · Conflictual relation   × 2.2     │
  │                                     │
  │  P(guilty | evidence) =             │
  │    P0 × Π(applicable factors)       │
  │    capped at 0.99                   │
  └──────────────┬──────────────────────┘
                 │
                 ▼
  Final Score = Tree × 0.6 + Bayes × 0.4
  Ranked list of suspects with explanation
```

### Why This Design

Each layer does something the others can't:

- **CSP** handles hard logic — a suspect with a verified alibi cannot be guilty regardless of any score. These are not probabilities, they are facts. The CSP eliminates them before any scoring begins.
- **Decision Tree** handles ordered reasoning — certain attributes (alibi, motive, force, physical evidence) are checked in a fixed priority order, mimicking how a detective would think through a case step by step.
- **Bayesian Network** handles uncertainty — not all evidence is binary. Multiple probabilistic factors are combined multiplicatively to produce a posterior probability, giving the system a way to weigh accumulated evidence rather than just flag binary matches.

---

## Prolog Knowledge Base (`sherlockk.pl`)

The entire domain is encoded in Prolog using predicate logic:

```prolog
% Fact: a suspect exists
suspect('John Doe').

% Fact: a characteristic of a suspect
caracteristique('John Doe', genre, homme).
caracteristique('John Doe', alibi, aucun).

% Fact: a piece of crime scene evidence
indice(arme, couteau).
indice(heure_crime, nuit).

% Rule: infer physical strength from attributes
inferer_force(Suspect, tres_elevee) :-
    caracteristique(Suspect, genre, homme),
    caracteristique(Suspect, age, adulte),
    caracteristique(Suspect, corpulence, athletique).

% Rule: CSP constraint — alibi eliminates suspect
contrainte_csp_unaire(Suspect) :-
    \+ alibi_excluant(Suspect).
```

The main query `coupable_hybride(Suspect, Score, Explication)` unifies all three layers and returns ranked results.

---

## Suspect Profile

Each suspect can be described across five attribute categories:

| Category | Attributes |
|----------|-----------|
| **Physical** | Shoe size, height, build, hair color |
| **Demographic** | Gender, age range, blood type |
| **Socio-economic** | Profession, financial status, education level |
| **Psychological** | Temperament, criminal record, mental stability, addiction |
| **Contextual** | Alibi, motive, relation to victim, proximity to scene |
| **Behavioural** | Nocturnal habits, weapon access, combat training, computer skills |

Attributes left blank are treated as unknown and do not trigger constraints.

---

## Crime Scene Evidence

| Evidence | Options |
|----------|---------|
| Footprint size | Number (30–50) |
| DNA gender | Male / Female / Unknown |
| Blood type | A+/A-/B+/B-/AB+/AB-/O+/O- |
| Hair color | Blonde / Brown / Red / Black / Gray |
| Weapon | Knife, pistol, axe, poison, strangulation, and more |
| Presumed motive | Financial, passion, revenge, pathological |
| Time of crime | Day / Night / Unknown |

---

## Project Structure

```
sherlock-expert-system/
├── app.py           # Flask backend — Prolog bridge, API routes
├── sherlockk.pl     # Prolog knowledge base — full AI logic
└── index.html       # Frontend — suspect registration + results UI
```

---

## Setup

**Requirements:** Python 3.10+, SWI-Prolog installed on the system

```bash
git clone https://github.com/your-username/sherlock-expert-system.git
cd sherlock-expert-system

pip install flask flask-cors pyswip

# Make sure SWI-Prolog is installed:
# Ubuntu/Debian: sudo apt install swi-prolog
# macOS:         brew install swi-prolog

python app.py
```

Open `index.html` directly in your browser (or serve it via the Flask route at `http://localhost:5000`).

---

## API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/add-suspect` | POST | Assert a suspect and their characteristics into Prolog memory |
| `/remove-suspect` | POST | Retract a suspect from Prolog memory |
| `/add-indices` | POST | Assert crime scene evidence into Prolog memory |
| `/run-inference` | POST | Run the hybrid engine — returns ranked suspects with scores |
| `/reset` | POST | Clear all suspects and evidence from Prolog memory |
| `/test-prolog` | GET | Verify the Prolog engine is loaded correctly |
| `/whats-in-prolog` | GET | Debug — dump current Prolog memory state |

---

## Built With

- [SWI-Prolog](https://www.swi-prolog.org/) — logic programming engine
- [PySwip](https://github.com/yuce/pyswip) — Python ↔ Prolog bridge
- [Flask](https://flask.palletsprojects.com/) — backend API
