#include <iostream>

template <typename Model>
class TimeAdapter {
  const double time_start;
  const double time_step;
  const double time_end;

  Model& model;           // actual model
  Model  point;           // point we are emitting
  double point_time;      // current time

public:
  TimeAdapter(double start, double step, double end, Model& model)
    : time_start(start), time_step(step), time_end(end),
      model(model), point(model)
  {
    this->step();
  }

  Model& getPoint() { return point; }
  double getTime() { return point_time; }

  bool done() { return point_time > time_end; }

  void step() {
    if(done())
      return;
    while(model.getNextTime() <= point_time + time_step)
      model.step();

    point = model;
    point_time = point_time + time_step;
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
    std::cout << "time = " << timed.getTime() << ", data = " << m.data << std::endl;
    timed.step();
  }


}

