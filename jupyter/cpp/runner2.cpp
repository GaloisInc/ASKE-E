#include<iostream>
#include<map>
#include"sir.cpp"

// run 10000 simulations and plot the distribution of the 
// time unit at which peak infection was reached 

int main(int argc, char** argv) {
  std::map<int, int> results;

  for(int i = 0; i < 10000; i++) {
    SIR sir;          // intialize the simulator
    sir.set_seed(i);  // use `i` as the seed for PRNG

    int peak = sir.state_I;
    int peak_day = sir.time;

    while(sir.time < 120.0) {
      int event;                 
      double nextEventTime;
      sir.next_event(event, nextEventTime);
      sir.run_event(event, nextEventTime);

      if(sir.state_I > peak) {
        peak = sir.state_I;
        peak_day = sir.time;
      }
    }

    if(results.find(peak_day) == results.end()) {
      results[peak_day] = 0;
    }
    results[peak_day] = results[peak_day] + 1;
  }

  std::cout << "time,peak_probability" << std::endl;
  for(auto const& kv : results) {
    std::cout << kv.first << "," << (kv.second / 10000.0) << std::endl;
  }
}
