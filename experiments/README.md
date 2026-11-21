# Quantitative Experiment & Knowledge Base Registry

This curated registry catalogs all verified mathematical models, empirical formulas, invariants, and rejected hypotheses across the 5-tier predictive architecture.

| Experiment ID | Tier | Title | Status | Key Formula / Metric | Updated |
|:---|:---:|:---|:---:|:---|:---|
| **[EXP-001-AERODYNAMICS](registry.json)** | 1 | Trajectory Aerodynamics, Approach Angles (VAA/HAA) & Whiff Lift | `VALIDATED` | `terminal_forward_velo`: vy_f = -sqrt(vy0^2 + 2 * ay * (1... | 2026-11-16 |
| **[EXP-002-PLATOON-PHYSICS](registry.json)** | 1 | 4-Quadrant Platoon Physics, Break Symmetry & Situational Pitching Effectiveness | `VALIDATED` | `hb_glove_in`: hb_in * (CASE WHEN p_throws = 'L' THEN -1 ... | 2026-11-18 |
| **[EXP-003-TUNNELING-STUFF-PLUS](registry.json)** | 1 | Commit-Point Pitch Tunneling & Platoon-Conditioned Stuff+ / Location+ / Pitching+ | `VALIDATED` | `commit_time_t_dec`: (-sqrt(vy0^2 + 2*ay*(23.8 - 50.0)) -... | 2026-11-19 |
| **[EXP-005-DECOMPOSED-PITCHING-DECISIONS](registry.json)** | 1 | Context-Neutral Markov Decomposed Pitching+ & Pitch-Level Batter Process Engine (Decision+, BatControl+, Contact+) | `VALIDATED` | `bip_rv_map`: delta_run_exp ~ factor(count_id) + 0.8593 *... | 2026-11-20 |
| **[EXP-004-PLAYER-PROJECTIONS](registry.json)** | 2 | Chronological True-Talent Player Projections: Marcel, Delta-Method Aging & Tier 1 Physics Priors | `VALIDATED` | `exact_ip_true`: FLOOR(IP) + ROUND((IP - FLOOR(IP)) * 10)... | 2026-11-20 |
| **[EXP-007-PECOTA-ZIPS-ENSEMBLE](registry.json)** | 2 | PECOTA k-NN Similarity Comps, ZiPS Archetype Multi-Year Aging, Defense/Baserunning EDA & 5-Paradigm Consensus Ensemble | `VALIDATED` | `era_neutral_zscore`: z_{i,s} = (x_{i,s} - mu_{s,g}) / sd... | 2026-11-21 |
| **[EXP-006-BALLPARK-FACTORS](registry.json)** | 3 | Empirical Multi-Year Regressed Ballpark Factors & Handedness Asymmetry | `VALIDATED` | `raw_scoring_ratio`: (r_home / g_home) / (r_road / g_road) | 2026-11-20 |
| **[EXP-008-TEAM-BASERUNS-PYTHAGENPAT](registry.json)** | 3 | Level 3 Team Run Creation (David Smyth BaseRuns), Dynamic Pythagenpat & 10,000-Season 2026 Standings Simulator | `VALIDATED` | `david_smyth_baseruns`: R = (A * B) / (B + C) + D | 2026-11-21 |

---
*Maintained by mlbr::lake_log_experiment()*
