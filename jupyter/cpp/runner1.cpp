#include<iostream>
#include"sir.cpp"

// runner that writes state variables as CSV
int main(int argc, char** argv) {
  SIR sir;             // initialize the simulator
  sir.set_seed(12345);  // set the RNG state

  // write out the header
  std::cout << "time,S,I,R" << std::endl;

  // run the model until 120 time units have elapsed, recording data as we go
  while(sir.time < 120.0) {
    // first we output the current state of the model
    std::cout << sir.time << ","
              << sir.state_S << ","
              << sir.state_I << ","
              << sir.state_R << std::endl;

    // next we run a step of model simulation
    // the proces of running a step of the simulation happens in two phases

    // first the `next_event` function uses a PRNG to select the next event and time
    // these two variables are passed by reference
    int event;                            // which event will happen?
    double nextEventTime;                 // when will it happen?
    sir.next_event(event, nextEventTime);

    // the `run_event` updates the simulation state
    sir.run_event(event, nextEventTime);
  }
}

