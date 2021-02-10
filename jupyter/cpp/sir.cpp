#include <random>
#include <stdint.h>
struct SIR {
  bool event_when_Infect() {
    return (0.0 < state_S) && (0.0 < state_I);
  }

  bool event_when_Remove() {
    return 0.0 < state_I;
  }

  double event_rate_Infect() {
    return ((0.4 * state_S) * state_I) / ((state_S + state_I) + state_R);
  }

  double event_rate_Remove() {
    return 4.0e-2 * state_I;
  }

  void event_effect_Infect() {
    state_I = state_I + 1.0;
    state_S = state_S - 1.0;
  }

  void event_effect_Remove() {
    state_I = state_I - 1.0;
    state_R = state_R + 1.0;
  }

  void next_event(int &nextEvent, double &nextTime) {
    double _eff_rate_Infect = event_when_Infect() ? event_rate_Infect() : 0.0;
    double _eff_rate_Remove = event_when_Remove() ? event_rate_Remove() : 0.0;
    double total_rate = _eff_rate_Infect + _eff_rate_Remove;
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
  }

  void run_event(int nextEvent, double nextTime) {
    time = nextTime;
    auto cur_state_I = state_I;
    auto cur_state_R = state_R;
    auto cur_state_S = state_S;
    switch (nextEvent) {
      case 1:
        state_I = cur_state_I + 1.0;
        state_S = cur_state_S - 1.0;
        break;
      case 2:
        state_I = cur_state_I - 1.0;
        state_R = cur_state_R + 1.0;
        break;
    }
  }

  void set_seed(uint32_t seed) {
    prng.seed(seed);
  }

  double state_I = 3.0;

  double state_R = 0.0;

  double state_S = 997.0;

  double time = 0.0;

  std::mt19937_64 prng;
};