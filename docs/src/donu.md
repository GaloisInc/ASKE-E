# Donu Documentation

## About Donu

Donu is the webservice that is able to run simulations and various kinds of analysis on models
for the ASKE-E program.

## Calling Convention

All operations are implemented at path `/` and are indifferent to HTTP verb
(`GET`, `POST`, etc.)

## Types

### `datasource`

A `datasource` is either an object with the single field `file` with :

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| file             | string                   | Filename to use as a data source                                  |

Example:

```JSON
{ "file": "model.gromet" }
```

Or a string containing the data itself:

```JSON
"model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n  let s_initial = 997..."
```

### `model-def`

A model def is a `datasource` along with a model type.  Valid model types are `easel`, `diff-eq` with `gromet` coming soon.

Example:

```
{
  "source": { "file": "modelRepo/easel/sir.easel" },
  "type": "easel"
}
```

## Operations

### `list-models` - List the available models


**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"list-models"`|

Example:

```JSON
{ "command": "list-models"}
```

**Response:**


| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| models           | list                     | list of model-def objects                                         |

Example:

```JSON
{
  "models": [
    {
      "source": { "file": "modelRepo/easel/sir.easel" },
      "type": "easel"
    }
  ]
}
```

### `simulate` - Simulate a model using ODEs

**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"simulate"`   |
| definition       | model-def                | Definition of the model                                           |
| start            | number                   | Start time of the simulation                                      |
| end              | number                   | End time of the simulation                                        |
| step             | number                   | Simulation time step size (optional, defaults to 1)               |
| parameters       | dictionary               | Use these values for model parameters (optional)                  |

Example:

```JSON
{
  "command": "simulate",
  "definition": {
    "type": "easel",
    "source": { "file": "modelRepo/easel/sir-meta.easel" }
  },
  "start": 0,
  "end": 120.0,
  "step": 30.0,
  "parameters": {
    "beta": 0.6
  }
}
```

**Response:**


| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| times            | list of number           | series of times used in simulation                                |
| values           | result series object     | values of state variables                                         |

The object in `values` is such that each key is the name of a model variable `V` and each value is a list `l` such that `V` has the value `l[x]` at time `times[x]`.

Example:

```JSON
{
  "values": {
    "I": [
      3,
      396.20766691080604,
      119.33588838987737,
      35.943279530151585,
      10.825907756942332
    ],
    "S": [
      997,
      0.0012550867795002216,
      2.4726152151230926e-06,
      3.7868257221162325e-07,
      2.151977671793774e-07
    ],
    "R": [
      0,
      603.7910780024147,
      880.6641091375077,
      964.0567200911661,
      989.1740920278602
    ]
  },
  "times": [
    0,
    30,
    60,
    90,
    120
  ]
}
```

### `get-model-schematic` - get schematic description of a model

This call gets a high level schematic description of a model as a graph.  Not all models support this visualization.

**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"get-model-schematic"`   |
| definition       | model-def                | Definition of the model                                           |

Example:

```JSON
{
  "command": "get-model-schematic",
  "definition": {
    "type": "easel",
    "source": { "file": "modelRepo/easel/sir.easel" }
  }
}
```

**Response:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| status           | string                   | `"success"` or `"failure"` depending on if the operation was succcesful   |
| result           | graph                    | Description of the schematic graph                                |


```JSON
{
  "status": "success",
  "result": {
    "nodes": [
      {
        "name": "Infect",
        "type": "event"
      },
      {
        "name": "S",
        "type": "state"
      },
      {
        "name": "I",
        "type": "state"
      },
      {
        "name": "Remove",
        "type": "event"
      },
      {
        "name": "R",
        "type": "state"
      }
    ],
    "edges": [
      [
        {
          "name": "Infect",
          "type": "event"
        },
        {
          "name": "I",
          "type": "state"
        }
      ],
      [
        {
          "name": "S",
          "type": "state"
        },
        {
          "name": "Infect",
          "type": "event"
        }
      ],
      [
        {
          "name": "I",
          "type": "state"
        },
        {
          "name": "Remove",
          "type": "event"
        }
      ],
      [
        {
          "name": "Remove",
          "type": "event"
        },
        {
          "name": "R",
          "type": "state"
        }
      ]
    ]
  }
}
```

### `get-model-source` - get source code for a model

**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"get-model-source"`   |
| definition       | model-def                | Definition of the model                                           |

```JSON
{
  "command": "get-model-source",
  "definition": {
    "type": "easel",
    "source": { "file": "modelRepo/easel/sir.easel" }
  }
}
```

**Response:**

The result, if successful, is a `model-def` object with the source inline.

```JSON
{
  "source": "model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n\n  let s_initial = 997\n  let i_initial = 3\n  let r_initial = 0\n\n  state S = s_initial\n  state I = i_initial\n  state R = r_initial\n\n  let total_population = S + I + R\n\n  event Infect:\n    when:\n      S > 0 and I > 0\n    rate: \n      beta * S * I / total_population\n    effect:\n      S -= 1\n      I += 1\n      \n  event Remove:\n    when:\n      I > 0\n    rate: \n      gamma * I\n    effect:\n      I -= 1\n      R += 1\n      \n      ",
  "type": "easel"
}
```

### `convert-model` - convert a model from one form to another

May fail if the conversion is not supported.

**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"convert-model"`   |
| definition       | model-def                | Definition of the model                                           |
| dest-type        | string                   | Model type to convert to - same fields `model-def`'s `type` field |

```JSON
{
  "command": "convert-model",
  "definition": {
    "type": "easel",
    "source": { "file": "modelRepo/easel/sir.easel" }
  },
  "dest-type":"diff-eqs"
}
```

**Response:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| status           | string                   | Either `success` or if the model conversion is not supported `failure` |
| result           | model-def                | Converted model (inline)                                          |

```JSON
{
  "status": "success",
  "result": {
    "source": "let beta = 0.4\nlet gamma = 0.04\nlet i_initial = 3.0\nlet r_initial = 0.0\nlet s_initial = 997.0\nlet total_population = S + I + R\nI(0) = 3.0\nR(0) = 0.0\nS(0) = 997.0\nd/dt I = (if 0.0 < S and 0.0 < I then 0.4 * S * I / (S + R + I) else 0.0) + (if 0.0 < I then -0.04 * I else 0.0)\nd/dt R = if 0.0 < I then 0.04 * I else 0.0\nd/dt S = if 0.0 < S and 0.0 < I then -0.4 * S * I / (S + R + I) else 0.0",
    "type": "diff-eqs"
  }
}
```

### `describe-model-interface` - describe parameters of a model

**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"describe-model-interface"`   |
| definition       | model-def                | Definition of the model                                           |

```JSON
{
  "command": "describe-model-interface",
  "definition": {
    "type": "easel",
    "source": {
      "file": "modelRepo/easel/sir-meta.easel"
    }
  }
}
```

**Response**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| status           | string                   | Either `success` or if the model conversion is not supported `failure` |
| result           | model-def                | a list of parameters and state variable                           |

```JSON
{
  "status": "success",
  "result": {
    "stateVars": [
      {
        "metadata": {
          "Description": "Susceptible population"
        },
        "name": "S"
      },
      {
        "metadata": {
          "Description": "Infected population"
        },
        "name": "I"
      },
      {
        "metadata": {
          "Description": "Recovered population"
        },
        "name": "R"
      }
    ],
    "parameters": [
      {
        "metadata": {
          "Description": "The average number of contacts per person per time, multiplied by the probability of disease transmission in a contact between a susceptible and an infectious subject"
        },
        "name": "beta",
        "defaultValue": 0.4
      },
      {
        "metadata": {
          "Description": "Rate of recovery from infection"
        },
        "name": "gamma",
        "defaultValue": 0.04
      },
      {
        "metadata": {
          "Description": "Initial population of suceptible people."
        },
        "name": "s_initial",
        "defaultValue": 997
      },
      {
        "metadata": {
          "Description": "Initial population of infected people."
        },
        "name": "i_initial",
        "defaultValue": 3
      },
      {
        "metadata": {
          "Description": "Initial population of recovered people."
        },
        "name": "r_initial",
        "defaultValue": 0
      }
    ]
  }
}
```

### `upload-model` - Upload a new model

This command may fail if you attempt to overwrite an existing model, or if the model you attempt to upload is not syntactically valid.

**Request:**

| Field            | Type                     | Description                                                        |
|------------------|--------------------------|--------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"upload-model"`|
| name             | string                   | The (base)name of the model in storage                             |
| type             | string                   | Model type - same options as `model-def`'s `type` field            |
| definition       | string                   | The model itself, inlined as a string                              |

Example:

```JSON
command = {
    "command": "upload-model",
    "name": "sir.easel",
    "type": "easel",
    "definition": "model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n\n  let s_initial = 997\n  let i_initial = 3\n  let r_initial = 0\n\n  state S = s_initial\n  state I = i_initial\n  state R = r_initial\n\n  let total_population = S + I + R\n\n  event Infect:\n    when:\n      S > 0 and I > 0\n    rate: \n      beta * S * I / total_population\n    effect:\n      S -= 1\n      I += 1\n      \n  event Remove:\n    when:\n      I > 0\n    rate: \n      gamma * I\n    effect:\n      I -= 1\n      R += 1\n      \n      "
}
```

**Response:**

On success:

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| status           | string                   | The literal `"success"`                                           |
| result           | model-def                | Definition of the resultant model                                 |

On failure:

| Field       | Type        | Description                   |
|-------------|-------------|-------------------------------|
| status      | string      | The literal `"error"`         |
| error       | string      | The reason for the error      |

Example:

```JSON
{
  "status": "success",
  "result": {
    "source": { "file": "modelRepo/easel/sir.easel" },
    "type": "easel"
  }
}
```

### `stratify-command` - Stratify a model

**Request**

| Field               | Type                     | Description                                                                                       |
|---------------------|--------------------------|---------------------------------------------------------------------------------------------------|
| command             | string                   | Command - for this operation it will be the string `"stratify-command"`                           |
| definition          | datasource               | The specification of the model                                                                    |
| connection-graph    | datasource               | JSON connection graph specifying stratification pattern                                           |
| state-metadata      | datasource               | JSON metadata describing infectious states (optional)                                             |
| stratification-type | string                   | The type of stratification to perform. The value should be either `"spatial"` or `"demographic"`. |

Example:

```JSON
command = {
  "command": "stratify-command",
  "definition": "model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n\n  let s_initial = 997...",
  "connection-graph": {
    "file": "/home/rscott/Documents/Hacking/Haskell/ASKE-E/jupyter/jupyter2/connectionGraph/cities.txt"
  },
  "stratification-type": "spatial"
}
```

**Response**

| Field            | Type                     | Description                                                 |
|------------------|--------------------------|-------------------------------------------------------------|
| raw-model        | string                   | The stratified model (inline)                               |
| pretty-model     | string                   | The stratified model, with some cosmetic touch-ups (inline) |
| topology         | dictionary               | The topology                                                |
| parameters       | array of strings         | List of parameters                                          |
| vertices         | object                   | Maps node indices to their names                            |

Example:

```JSON
{
  "raw-model": "model foo:\n  state S_1 = hole_A\n  state I_1 = hole_B\n  state R_1 = hole_C...",
  "pretty-model": "model foo:\n  state S_Portland = S_Portland_initial\n  state I_Portland = I_Portland_initial\n  state R_Portland = R_Portland_initial...",
  "topology": {
    "O": [
      {
        "os": 4,
        "ot": 1
      },
      {
        "os": 5,
        "ot": 2
      },
      {
        "os": 6,
        "ot": 3
      },
      {
        "os": 1,
        "ot": 4
      },
      {
        "os": 2,
        "ot": 5
      },
      {
        "os": 3,
        "ot": 6
      },
      {
        "os": 2,
        "ot": 7
      },
      {
        "os": 3,
        "ot": 8
      },
      {
        "os": 5,
        "ot": 9
      },
      {
        "os": 6,
        "ot": 10
      }
    ],
    "T": [
      {
        "tname": "diff_S_1_S_2"
      },
      {
        "tname": "diff_I_1_I_2"
      },
      {
        "tname": "diff_R_1_R_2"
      },
      {
        "tname": "diff_S_2_S_1"
      },
      {
        "tname": "diff_I_2_I_1"
      },
      {
        "tname": "diff_R_2_R_1"
      },
      {
        "tname": "Infect_1"
      },
      {
        "tname": "Remove_1"
      },
      {
        "tname": "Infect_2"
      },
      {
        "tname": "Remove_2"
      }
    ],
    "I": [
      {
        "it": 1,
        "is": 1
      },
      {
        "it": 2,
        "is": 2
      },
      {
        "it": 3,
        "is": 3
      },
      {
        "it": 4,
        "is": 4
      },
      {
        "it": 5,
        "is": 5
      },
      {
        "it": 6,
        "is": 6
      },
      {
        "it": 7,
        "is": 1
      },
      {
        "it": 8,
        "is": 2
      },
      {
        "it": 9,
        "is": 4
      },
      {
        "it": 10,
        "is": 5
      }
    ],
    "S": [
      {
        "sname": "S_1"
      },
      {
        "sname": "I_1"
      },
      {
        "sname": "R_1"
      },
      {
        "sname": "S_2"
      },
      {
        "sname": "I_2"
      },
      {
        "sname": "R_2"
      }
    ]
  },
  "parameters": [
    "hole_A",
    "hole_B",
    "hole_C",
    "hole_D",
    "hole_E",
    "hole_F",
    "hole_G",
    "hole_H",
    "hole_I",
    "hole_J",
    "hole_K",
    "hole_L",
    "hole_M",
    "hole_N",
    "hole_O",
    "hole_P"
  ],
  "vertices": {
    "2": "Seattle",
    "1": "Portland"
  }
}
```

### `generate-cpp` - Render a model as C++ program implementing a simulator

**Request**

| Field               | Type                     | Description                                                         |
|---------------------|--------------------------|---------------------------------------------------------------------|
| command             | string                   | Command - for this operation it will be the string `"generate-cpp"` |
| definition          | model-def                | Specification of the model to render                                |

Example:

```JSON
command = {
  "command": "generate-cpp",
  "definition": {
    "source": "model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n\n  let s_initial = 997...",
    "type": "easel"
  }
}
```

**Response**

On success:

| Field            | Type                     | Description                    |
|------------------|--------------------------|--------------------------------|
| status           | string                   | The literal `"success"`        |
| result           | string                   | The C++ code for the simulator |

On failure:

| Field       | Type        | Description                   |
|-------------|-------------|-------------------------------|
| status      | string      | The literal `"error"`         |
| error       | string      | The reason for the error      |

Example:

```JSON
{
  "status": "success",
  "result": "#include <random>\n#include <stdint.h>\n#include <math.h>\nstruct SIR..."
}
```

### `check-model` - Validate that a model is runnable

**Request**

| Field               | Type                     | Description                                                        |
|---------------------|--------------------------|--------------------------------------------------------------------|
| command             | string                   | Command - for this operation it will be the string `"check-model"` |
| definition          | model-def                | Specification of the model                                         |

Example:

```JSON
command = {
  "command": "generate-cpp",
  "definition": {
    "source": "model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n\n  let s_initial = 997...",
    "type": "easel"
  }
}
```

**Response**

On success:

| Field            | Type                     | Description             |
|------------------|--------------------------|-------------------------|
| status           | string                   | The literal `"success"` |
| result           | array                    | An empty array          |

On failure:

| Field       | Type        | Description                   |
|-------------|-------------|-------------------------------|
| status      | string      | The literal `"error"`         |
| error       | string      | The reason for the error      |

Example:

```JSON
{
  "status": "success",
  "result": []
}
```
