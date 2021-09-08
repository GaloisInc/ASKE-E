#include <random>
#include <stdint.h>
#include <math.h>
struct SIRS {
  bool event_when_Infect() {
    return (0.0 < S) && (0.0 < I);
  }

  bool event_when_Recover() {
    return 0.0 < I;
  }

  bool event_when_Die() {
    return 0.0 < I;
  }

  bool event_when_Relapse() {
    return 0.0 < R;
  }

  double event_rate_Infect() {
    return ((0.4 * S) * I) / ((S + R) + I);
  }

  double event_rate_Recover() {
    return 4.0e-2 * I;
  }

  double event_rate_Die() {
    return 1.0e-2 * I;
  }

  double event_rate_Relapse() {
    return 5.0e-2 * R;
  }

  void event_effect_Infect() {
    I = I + 1.0;
    S = S - 1.0;
  }

  void event_effect_Recover() {
    I = I - 1.0;
    R = R + 1.0;
  }

  void event_effect_Die() {
    D = D + 1.0;
    I = I - 1.0;
  }

  void event_effect_Relapse() {
    R = R - 1.0;
    S = S + 1.0;
  }

  void next_event(int &nextEvent, double &nextTime) {
    double _eff_rate_Infect = event_when_Infect() ? event_rate_Infect() : 0.0;
    double _eff_rate_Recover = event_when_Recover() ? event_rate_Recover() : 0.0;
    double _eff_rate_Die = event_when_Die() ? event_rate_Die() : 0.0;
    double _eff_rate_Relapse = event_when_Relapse() ? event_rate_Relapse() : 0.0;
    double total_rate = _eff_rate_Infect + _eff_rate_Recover + _eff_rate_Die + _eff_rate_Relapse;
    auto rate_dist = std::uniform_real_distribution<double>{0.0, total_rate};
    auto dt_dist = std::exponential_distribution<double>{total_rate};
    double random = rate_dist(prng);
    nextTime = time + dt_dist(prng);
    random -= _eff_rate_Infect;
    if (random <= 0.0) {
      nextEvent = 1;
      return;
    }
    random -= _eff_rate_Recover;
    if (random <= 0.0) {
      nextEvent = 2;
      return;
    }
    random -= _eff_rate_Die;
    if (random <= 0.0) {
      nextEvent = 3;
      return;
    }
    random -= _eff_rate_Relapse;
    if (random <= 0.0) {
      nextEvent = 4;
      return;
    }
  }

  void run_event(int nextEvent, double nextTime) {
    time = nextTime;
    auto cur_D = D;
    auto cur_I = I;
    auto cur_R = R;
    auto cur_S = S;
    switch (nextEvent) {
      case 1:
        I = cur_I + 1.0;
        S = cur_S - 1.0;
        break;
      case 2:
        I = cur_I - 1.0;
        R = cur_R + 1.0;
        break;
      case 3:
        D = cur_D + 1.0;
        I = cur_I - 1.0;
        break;
      case 4:
        R = cur_R - 1.0;
        S = cur_S + 1.0;
        break;
    }
  }

  void set_seed(uint32_t seed) {
    prng.seed(seed);
  }

  double D = 0.0;

  double I = 3.0;

  double R = 0.0;

  double S = 997.0;

  double time = 0.0;

  std::mt19937_64 prng;
};