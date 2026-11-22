# Quantitative Experiment & Knowledge Base Registry

This curated registry catalogs all verified mathematical models, empirical formulas, invariants, and rejected hypotheses across the 5-tier predictive architecture.

| Experiment ID | Tier | Title | Status | Key Formula / Metric | Updated |
|:---|:---:|:---|:---:|:---|:---|
| **[EXP-001-AERODYNAMICS](registry.json)** | 1 | Trajectory Aerodynamics, Approach Angles (VAA/HAA) & Whiff Lift | `VALIDATED` | `terminal_forward_velo`: vy_f = -sqrt(vy0^2 + 2 * ay * (1... | 2026-11-16 |
| **[EXP-002-PLATOON-PHYSICS](registry.json)** | 1 | 4-Quadrant Platoon Physics, Break Symmetry & Situational Pitching Effectiveness | `VALIDATED` | `hb_glove_in`: hb_in * (CASE WHEN p_throws = 'L' THEN -1 ... | 2026-11-18 |
| **[EXP-003-TUNNELING-STUFF-PLUS](registry.json)** | 1 | Commit-Point Pitch Tunneling & Platoon-Conditioned Stuff+ / Location+ / Pitching+ | `VALIDATED` | `commit_time_t_dec`: (-sqrt(vy0^2 + 2*ay*(23.8 - 50.0)) -... | 2026-11-19 |
| **[EXP-005-DECOMPOSED-PITCHING-DECISIONS](registry.json)** | 1 | Context-Neutral Markov Decomposed Pitching+ & Pitch-Level Batter Process Engine (Decision+, BatControl+, Contact+) | `VALIDATED` | `bip_rv_map`: delta_run_exp ~ factor(count_id) + 0.8593 *... | 2026-11-20 |
| **[EXP-011-BAT-TRACKING-ARM-SLOT-BIOMECHANICS](registry.json)** | 1 | Hawk-Eye Bat-Tracking (1.15M Swings) & Pitcher Arm-Slot (arm_angle) Biomechanics | `VALIDATED` | `max_ev_collision`: MaxEV = 1.23 * bat_speed + 0.2116 * r... | 2026-11-22 |
| **[EXP-004-PLAYER-PROJECTIONS](registry.json)** | 2 | Chronological True-Talent Player Projections: Marcel, Delta-Method Aging & Tier 1 Physics Priors | `VALIDATED` | `exact_ip_true`: FLOOR(IP) + ROUND((IP - FLOOR(IP)) * 10)... | 2026-11-20 |
| **[EXP-007-PECOTA-ZIPS-ENSEMBLE](registry.json)** | 2 | PECOTA k-NN Similarity Comps, ZiPS Archetype Multi-Year Aging, Defense/Baserunning EDA & 5-Paradigm Consensus Ensemble | `VALIDATED` | `era_neutral_zscore`: z_{i,s} = (x_{i,s} - mu_{s,g}) / sd... | 2026-11-21 |
| **[EXP-006-BALLPARK-FACTORS](registry.json)** | 3 | Empirical Multi-Year Regressed Ballpark Factors & Handedness Asymmetry | `VALIDATED` | `raw_scoring_ratio`: (r_home / g_home) / (r_road / g_road) | 2026-11-20 |
| **[EXP-008-TEAM-BASERUNS-PYTHAGENPAT](registry.json)** | 3 | Level 3 Team Run Creation (David Smyth BaseRuns), Dynamic Pythagenpat & 10,000-Season 2026 Standings Simulator | `VALIDATED` | `david_smyth_baseruns`: R = (A * B) / (B + C) + D | 2026-11-21 |
| **[EXP-012-WEATHER-UMPIRE-CATCHER-FRAMING](registry.json)** | 3 | Environmental & Officiating Alpha: Weather/Wind Carry, Home-Plate Umpire Strike Zones & Catcher Framing (CSAA) | `VALIDATED` | `weather_carry`: R_env = b0 + b1*PF_runs + b2*(Temp - 70)... | 2026-11-22 |
| **[EXP-013-PITCHER-STAMINA-BULLPEN-FATIGUE](registry.json)** | 3 | Pitcher Stamina, In-Game Fastball Velocity Decay & Dynamic Rolling Bullpen Fatigue Engine | `VALIDATED` | `velo_decay`: Delta_v(k) = v(k) - mean(v_1..20) | 2026-11-22 |
| **[EXP-009-GAME-MONTE-CARLO-SIMULATOR](registry.json)** | 4 | Level 4 Plate-Appearance 24 Base-Out Markov Game Simulator & Empirical Win Expectancy Engine | `VALIDATED` | `multi_category_log5`: P(E) = (P_bat(E) * P_pit(E) / P_lg... | 2026-11-21 |
| **[EXP-010-MARKET-EDGE-PREDICTION-VALUATION](registry.json)** | 5 | Level 5 Market Edge, Closing Line Value (CLV), Fractional Kelly Bankroll Optimization & Prediction Market Valuation | `VALIDATED` | `expected_value`: EV = p * (O - 1) - (1 - p) = p * O - 1 | 2026-11-22 |
| **[EXP-014-UNIFIED-PRO-GAME-F5-TOTALS-BRACKET](registry.json)** | 5 | Unified Pro Multi-Market Engine (Full-Game ML, F5 ML, Totals) & Exact 12-Team MLB Playoff Bracket Simulator | `VALIDATED` | `unified_ml`: logit(p_ML) = b0 + b1*logit(p_M3) + b2*logi... | 2026-11-22 |
| **[EXP-015-DAILY-LINEUPS-PLATOON-K-PROPS](registry.json)** | 5 | Daily 9-Man Starting Lineups, Handedness Park Asymmetry & Starting Pitcher Strikeout (SO) Prop Pricing Engine | `VALIDATED` | `lineup_k_pct`: LineupK% = sum(w_slot * EB_K%_prior) / su... | 2026-11-22 |

---
*Maintained by mlbr::lake_log_experiment()*
