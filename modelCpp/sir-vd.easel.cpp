#include <random>
#include <stdint.h>
#include <math.h>
struct SIR_VD {
  bool event_when_Infect() {
    return (0.0 < S) && (0.0 < I);
  }

  bool event_when_Remove() {
    return 0.0 < I;
  }

  bool event_when_Birth() {
    return true;
  }

  bool event_when_SDeath() {
    return 0.0 < S;
  }

  bool event_when_IDeath() {
    return 0.0 < I;
  }

  bool event_when_RDeath() {
    return 0.0 < R;
  }

  double event_rate_Infect() {
    return ((0.4 * I) * S) / ((S + R) + I);
  }

  double event_rate_Remove() {
    return 4.0e-2 * I;
  }

  double event_rate_Birth() {
    return 1.5e-3 * ((S + R) + I);
  }

  double event_rate_SDeath() {
    return 1.0e-3 * S;
  }

  double event_rate_IDeath() {
    return 1.0e-3 * I;
  }

  double event_rate_RDeath() {
    return 1.0e-3 * R;
  }

  void event_effect_Infect() {
    I = I + 1.0;
    S = S - 1.0;
  }

  void event_effect_Remove() {
    I = I - 1.0;
    R = R + 1.0;
  }

  void event_effect_Birth() {
    S = S + 1.0;
  }

  void event_effect_SDeath() {
    S = S - 1.0;
  }

  void event_effect_IDeath() {
    I = I - 1.0;
  }

  void event_effect_RDeath() {
    R = R - 1.0;
  }

  void next_event(int &nextEvent, double &nextTime) {
    double _eff_rate_Infect = event_when_Infect() ? event_rate_Infect() : 0.0;
    double _eff_rate_Remove = event_when_Remove() ? event_rate_Remove() : 0.0;
    double _eff_rate_Birth = event_when_Birth() ? event_rate_Birth() : 0.0;
    double _eff_rate_SDeath = event_when_SDeath() ? event_rate_SDeath() : 0.0;
    double _eff_rate_IDeath = event_when_IDeath() ? event_rate_IDeath() : 0.0;
    double _eff_rate_RDeath = event_when_RDeath() ? event_rate_RDeath() : 0.0;
    double total_rate = _eff_rate_Infect + _eff_rate_Remove + _eff_rate_Birth + _eff_rate_SDeath + _eff_rate_IDeath + _eff_rate_RDeath;
    auto rate_dist = std::uniform_real_distribution<double>{0.0, total_rate};
    auto dt_dist = std::exponential_distribution<double>{total_rate};
    double random = rate_dist(prng);
    nextTime = time + dt_dist(prng);
    random -= _eff_rate_Infect;
    if (random <= 0.0) {
      nextEvent = 1;
      return;
    }
    random -= _eff_rate_Remove;
    if (random <= 0.0) {
      nextEvent = 2;
      return;
    }
    random -= _eff_rate_Birth;
    if (random <= 0.0) {
      nextEvent = 3;
      return;
    }
    random -= _eff_rate_SDeath;
    if (random <= 0.0) {
      nextEvent = 4;
      return;
    }
    random -= _eff_rate_IDeath;
    if (random <= 0.0) {
      nextEvent = 5;
      return;
    }
    random -= _eff_rate_RDeath;
    if (random <= 0.0) {
      nextEvent = 6;
      return;
    }
  }

  void run_event(int nextEvent, double nextTime) {
    time = nextTime;
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
        S = cur_S + 1.0;
        break;
      case 4:
        S = cur_S - 1.0;
        break;
      case 5:
        I = cur_I - 1.0;
        break;
      case 6:
        R = cur_R - 1.0;
        break;
    }
  }

  void set_seed(uint32_t seed) {
    prng.seed(seed);
  }

  double I = 3.0;

  double R = 0.0;

  double S = 997.0;

  double time = 0.0;

  std::mt19937_64 prng;
};