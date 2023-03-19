// https://www.youtube.com/watch?v=PvA3RgwMDNM&t=1457s
// TODO annotate everything with comment :|

import 'dart:math';

class Network {
  List<Layer> layers;

  Network(this.layers);

  List<double> forward(List<double> inputs) {
    List<double> current = inputs;
    for (final layer in layers) {
      // Take the result from the propagation.
      current = layer.forward(inputs);
    }
    return current;
  }
}

class Layer {
  List<Neuron> neurons;

  Layer(this.neurons);

  List<double> forward(List<double> inputs) {
    return neurons.map((neuron) => neuron.forward(inputs)).toList();
  }
}

class Neuron {
  List<double> weights;

  double bias;

  Neuron(this.weights, this.bias);

  double forward(List<double> inputs) {
    assert(inputs.length == weights.length);
    double result = 0.0;
    for (int i = 0; i < inputs.length; i++) {
      result += inputs[i] * weights[i];
    }

    result = max(0, result);

    return result += bias;
  }
}
