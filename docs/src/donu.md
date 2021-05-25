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
| step             | number                   | Simulation time step size                                         |

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
| values           | result series object     | values of state varaibles                                         |

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