# Specification 09: Level 4 Plate-Appearance Markov Monte Carlo Game Simulator & Live Win Probability

- **Experiment ID:** `EXP-009-GAME-MONTE-CARLO-SIMULATOR`
- **Architecture Tier:** Level 4 (Game-Level Win Probability, Inning Simulation & In-Game Leverage Engine)
- **Status:** `VALIDATED`
- **Upstream Dependencies:**
  - Level 2 Gold Tables: `data/processed/pitcher_projections_true_talent.parquet` (`EXP-004`), `data/processed/batter_projections_true_talent.parquet` (`EXP-004`)
  - Level 3 Gold Tables: `data/processed/ballpark_factors_2002_2025.parquet` (`EXP-006`), `data/processed/team_baseruns_pythagenpat_2002_2025.parquet` (`EXP-007`)
  - Level 1 Physics Priors: 4-Quadrant Platoon & TTO degradation curves (`EXP-002`)
  - Bronze / Raw Views: `v_retrosheet_games`, `v_retrosheet_plays`, `v_mlbmeta_schedule`
- **Downstream Consumers:** Level 5 Prediction Market Valuation (Kalshi, Polymarket, Game Moneylines, Run Lines, In-Game Win Probabilities).

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Matchup Conditioning via Normalized Log5 Odds Ratio
To simulate any individual plate appearance between batter $i$ and pitcher $j$, we apply Tom Tango's generalized multi-category **Log5 odds ratio**:
$$P_{ij}(E) = \frac{\frac{P_i(E) \cdot P_j(E)}{P_{\text{lg}}(E)}}{\sum_{k \in \mathcal{E}} \frac{P_i(E_k) \cdot P_j(E_k)}{P_{\text{lg}}(E_k)}}$$
for outcomes $\mathcal{E} \in \{\text{SO}, \text{BB}, \text{HR}, \text{1B}, \text{2B}, \text{3B}, \text{Out\_in\_Play}\}$.

### 1.2 State Transition Matrix & Base-Out Mechanics
The game state is defined by the discrete Markov tuple:
$$S_t = (\text{Inning}, \text{Half}, \text{Outs} \in \{0,1,2\}, \text{Bases} \in \{000, 100, 020, 003, 120, 103, 023, 123\}, \text{Score\_Diff})$$
- Baserunner advancement follows empirical conditional probabilities (from `v_retrosheet_plays` empirical transition frequencies):
  - Runner on 1st on single: $\Pr(\text{1st to 3rd}) = 0.285$, $\Pr(\text{1st to 2nd}) = 0.710$, $\Pr(\text{Out at 3rd}) = 0.005$.
  - Runner on 2nd on single: $\Pr(\text{Scores}) = 0.590$, $\Pr(\text{Holds at 3rd}) = 0.405$.
- Pitcher Fatigue / Times Through the Order (TTO):
  - 1st TTO: Base talent ($1.00\times$).
  - 2nd TTO: $+0.015$ wOBA allowed.
  - 3rd TTO: $+0.038$ wOBA allowed.
  - Bullpen Hand-off: Pitch count threshold ($N \approx 85\text{--}100$ pitches) triggers dynamic bullpen substitution based on leverage index.

### 1.3 Ballpark & Handedness Multipliers
Outcome event rates are scaled by the venue's Gold 3-year regressed ballpark factors:
$$P_{ij,\text{park}}(\text{HR}) = P_{ij}(\text{HR}) \times \frac{PF_{\text{hr}}(\text{Hand})}{100}$$
where $\text{Hand} \in \{\text{LHB}, \text{RHB}\}$.

---

## 2. Validation & Benchmark Strategy
- **Historical Backtest (2024–2025 Regular Season Games, $N = 4,860$ games):**
  - Compare simulated pre-game win probabilities against:
    - $B_0$: 50/50 Coin Flip (Brier Score = 0.2500)
    - $B_1$: Home Field Advantage Constant (53.8% home win)
    - $B_2$: Elo / Pythagorean Rating Model
    - $M_4$: Plate-Appearance Monte Carlo Simulator (10,000 paths/game)
  - Evaluate:
    - Brier Score: $\frac{1}{N} \sum (p_i - y_i)^2$
    - Log-Loss: $-\frac{1}{N} \sum [y_i \log p_i + (1-y_i) \log(1-p_i)]$
    - Calibration Slope & Intercept: Logistic regression of actual win on $\text{logit}(p_i)$.

---

## 3. Implementation Deliverables
- Model: `models/09_game_monte_carlo_simulator.R`
- Output: `data/processed/game_simulation_backtest_2024_2025.parquet`
- Study: `site/analyses/09_game_monte_carlo_simulator.qmd`
- Lab: `workbooks/lab09_game_monte_carlo_simulator_workbook.R` & `site/workbooks/lab09_game_monte_carlo_simulator_workbook.qmd`
- Visuals: Dark Cyber 4-asset suites (`models/mc01_*`, `models/mc02_*`).

