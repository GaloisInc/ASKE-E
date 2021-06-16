#include <random>
#include <stdint.h>
#include <math.h>
struct SEIRS_Hospitalization_Death {
  bool event_when_Expose() {
    return true;
  }

  bool event_when_InfectedRecover() {
    return true;
  }

  bool event_when_HospitalizedRecover() {
    return true;
  }

  bool event_when_InfectedDie() {
    return true;
  }

  bool event_when_HospitalizedDie() {
    return true;
  }

  bool event_when_Infect() {
    return true;
  }

  bool event_when_Hospitalize() {
    return true;
  }

  double event_rate_Expose() {
    return ((((0.8 * Infected) * Susceptible) / ((((Susceptible + Recovered) + Infected) + Hospitalized) + Exposed)) - (0.6 * Exposed)) - (5.0e-3 * Exposed);
  }

  double event_rate_InfectedRecover() {
    return ((0.333 * Infected) - (0.4 * Recovered)) - (5.0e-3 * Recovered);
  }

  double event_rate_HospitalizedRecover() {
    return ((0.333 * Hospitalized) - (0.4 * Recovered)) - (5.0e-3 * Recovered);
  }

  double event_rate_InfectedDie() {
    return 1.0e-2 * Infected;
  }

  double event_rate_HospitalizedDie() {
    return 5.0e-2 * Hospitalized;
  }

  double event_rate_Infect() {
    return ((0.48 * Exposed) - (0.333 * Infected)) - (1.0e-2 * Infected);
  }

  double event_rate_Hospitalize() {
    return (((0.12 * Exposed) + - (Hospitalized)) - 0.333) - (5.0e-2 * Hospitalized);
  }

  void event_effect_Expose() {
    Exposed = Exposed + 1.0;
    Susceptible = Susceptible - 1.0;
  }

  void event_effect_InfectedRecover() {
    Infected = Infected - 1.0;
    Recovered = Recovered + 1.0;
  }

  void event_effect_HospitalizedRecover() {
    Hospitalized = Hospitalized - 1.0;
    Recovered = Recovered + 1.0;
  }

  void event_effect_InfectedDie() {
    Dead = Dead + 1.0;
    Infected = Infected - 1.0;
  }

  void event_effect_HospitalizedDie() {
    Dead = Dead + 1.0;
    Hospitalized = Hospitalized - 1.0;
  }

  void event_effect_Infect() {
    Exposed = Exposed - 1.0;
    Infected = Infected + 1.0;
  }

  void event_effect_Hospitalize() {
    Exposed = Exposed - 1.0;
    Hospitalized = Hospitalized + 1.0;
  }

  void next_event(int &nextEvent, double &nextTime) {
    double _eff_rate_Expose = event_when_Expose() ? event_rate_Expose() : 0.0;
    double _eff_rate_InfectedRecover = event_when_InfectedRecover() ? event_rate_InfectedRecover() : 0.0;
    double _eff_rate_HospitalizedRecover = event_when_HospitalizedRecover() ? event_rate_HospitalizedRecover() : 0.0;
    double _eff_rate_InfectedDie = event_when_InfectedDie() ? event_rate_InfectedDie() : 0.0;
    double _eff_rate_HospitalizedDie = event_when_HospitalizedDie() ? event_rate_HospitalizedDie() : 0.0;
    double _eff_rate_Infect = event_when_Infect() ? event_rate_Infect() : 0.0;
    double _eff_rate_Hospitalize = event_when_Hospitalize() ? event_rate_Hospitalize() : 0.0;
    double total_rate = _eff_rate_Expose + _eff_rate_InfectedRecover + _eff_rate_HospitalizedRecover + _eff_rate_InfectedDie + _eff_rate_HospitalizedDie + _eff_rate_Infect + _eff_rate_Hospitalize;
    auto rate_dist = std::uniform_real_distribution<double>{0.0, total_rate};
    auto dt_dist = std::exponential_distribution<double>{total_rate};
    double random = rate_dist(prng);
    nextTime = time + dt_dist(prng);
    random -= _eff_rate_Expose;
    if (random <= 0.0) {
      nextEvent = 1;
      return;
    }
    random -= _eff_rate_InfectedRecover;
    if (random <= 0.0) {
      nextEvent = 2;
      return;
    }
    random -= _eff_rate_HospitalizedRecover;
    if (random <= 0.0) {
      nextEvent = 3;
      return;
    }
    random -= _eff_rate_InfectedDie;
    if (random <= 0.0) {
      nextEvent = 4;
      return;
    }
    random -= _eff_rate_HospitalizedDie;
    if (random <= 0.0) {
      nextEvent = 5;
      return;
    }
    random -= _eff_rate_Infect;
    if (random <= 0.0) {
      nextEvent = 6;
      return;
    }
    random -= _eff_rate_Hospitalize;
    if (random <= 0.0) {
      nextEvent = 7;
      return;
    }
  }

  void run_event(int nextEvent, double nextTime) {
    time = nextTime;
    auto cur_Dead = Dead;
    auto cur_Exposed = Exposed;
    auto cur_Hospitalized = Hospitalized;
    auto cur_Infected = Infected;
    auto cur_Recovered = Recovered;
    auto cur_Susceptible = Susceptible;
    switch (nextEvent) {
      case 1:
        Exposed = cur_Exposed + 1.0;
        Susceptible = cur_Susceptible - 1.0;
        break;
      case 2:
        Infected = cur_Infected - 1.0;
        Recovered = cur_Recovered + 1.0;
        break;
      case 3:
        Hospitalized = cur_Hospitalized - 1.0;
        Recovered = cur_Recovered + 1.0;
        break;
      case 4:
        Dead = cur_Dead + 1.0;
        Infected = cur_Infected - 1.0;
        break;
      case 5:
        Dead = cur_Dead + 1.0;
        Hospitalized = cur_Hospitalized - 1.0;
        break;
      case 6:
        Exposed = cur_Exposed - 1.0;
        Infected = cur_Infected + 1.0;
        break;
      case 7:
        Exposed = cur_Exposed - 1.0;
        Hospitalized = cur_Hospitalized + 1.0;
        break;
    }
  }

  void set_seed(uint32_t seed) {
    prng.seed(seed);
  }

  double Dead = 0.0;

  double Exposed = 0.0;

  double Hospitalized = 0.0;

  double Infected = 1.0;

  double Recovered = 0.0;

  double Susceptible = 500.0;

  double time = 0.0;

  std::mt19937_64 prng;
};