#include <iostream>

template <typename Model>
class TimeAdapter {
  const double time_start;
  const double time_step;
  const double time_end;

  Model& model;           // actual model
  Model  point;           // point we are emitting
  double point_time;      // current time

  // assumes that model.time <= point_time
  void advanceToPointTime() {
    while (model.getNextTime() < point_time) model.step();
    point = model;
  }

public:
  TimeAdapter(double start, double step, double end, Model& model)
    : time_start(start), time_step(step), time_end(end),
      model(model), point(model), point_time(start)
  { advanceToPointTime();
  }

  Model& getPoint() { return point; }
  bool done() { return point_time > time_end; }


  void step() {
    if (point_time > time_end) return;
    point_time += time_step;
    if (point_time > time_end) return;
    if (point_time <= model.getTime()) return;
    advanceToPointTime();
  }


};

struct Example {
  double time;
  double data;

  Example() : time(0), data(0) {}

  double getTime() { return time; }

  double getNextTime() { return time + 5; }

  void step() {
    time += 5;
    data = 2 * time;
  }

};

int main() {
  Example model;
  TimeAdapter<Example> timed(0,1,120,model);

  while (!timed.done()) {
    Example &m = timed.getPoint();
    std::cout << "time = " << m.time << ", data = " << m.data << std::endl;
    timed.step();
  }


}

