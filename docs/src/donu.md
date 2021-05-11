# Donu Documentation

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
    "source": { "file": "modelRepo/easel/sir.easel" }
  },
  "start": 0,
  "end": 120.0,
  "step": 30.0
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
      570.9758710681082,
      177.87795797377797,
      53.663601453388395,
      16.17524903479719
    ],
    "S": [
      997,
      16.03663576555767,
      0.2688016239687885,
      7.747202089688689e-2,
      5.323898868597058e-2
    ],
    "R": [
      0,
      412.9874931663346,
      821.8532404022534,
      946.258926525715,
      983.771511976517
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

## `get-model-schematic` - get schematic description of a model

This call gets a high level schematic description of a model as a graph.  Not all models support this visualization.

**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"get-model-schematic"`   |
| definition       | model-def                | Definition of the model                                           |

Example:

```
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


```
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

## `get-model-source` - get source code for a model

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

## `convert-model` - convert a model from one form to another

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