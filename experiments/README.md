# Quantitative Experiment & Knowledge Base Registry

This curated registry catalogs all verified mathematical models, empirical formulas, invariants, and rejected hypotheses across the 5-tier predictive architecture.

| Experiment ID | Tier | Title | Status | Key Formula / Metric | Updated |
|:---|:---:|:---|:---:|:---|:---|
| **[EXP-001-AERODYNAMICS](registry.json)** | 1 | Trajectory Aerodynamics, Approach Angles (VAA/HAA) & Whiff Lift | `VALIDATED` | `terminal_forward_velo`: vy_f = -sqrt(vy0^2 + 2 * ay * (1... | 2026-11-16 |
| **[EXP-002-PLATOON-PHYSICS](registry.json)** | 1 | 4-Quadrant Platoon Physics, Break Symmetry & Situational Pitching Effectiveness | `VALIDATED` | `hb_glove_in`: hb_in * (CASE WHEN p_throws = 'L' THEN -1 ... | 2026-11-18 |
| **[EXP-003-TUNNELING-STUFF-PLUS](registry.json)** | 1 | Commit-Point Pitch Tunneling & Platoon-Conditioned Stuff+ / Location+ / Pitching+ | `VALIDATED` | `commit_time_t_dec`: (-sqrt(vy0^2 + 2*ay*(23.8 - 50.0)) -... | 2026-11-19 |
| **[EXP-004-PLAYER-PROJECTIONS](registry.json)** | 2 | Chronological True-Talent Player Projections: Marcel, Delta-Method Aging & Tier 1 Physics Priors | `VALIDATED` | `exact_ip_true`: FLOOR(IP) + ROUND((IP - FLOOR(IP)) * 10)... | 2026-11-20 |

---
*Maintained by mlbr::lake_log_experiment()*
