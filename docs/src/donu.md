# Donu Documentation

## About Donu

Donu is the webservice that is able to run simulations and various kinds of analysis on models
for the ASKE-E program.

## Calling Convention

All operations are implemented at path `/` and are indifferent to HTTP verb 
(`GET`, `POST`, etc.)

## Response Convention
_All_ responses return a two-element dict. 

On success, the members are `"status"`, which will map to the literal string `"success"`, and `"result"`, which will map to a dict containing whatever is specified below in a command's **Response** section.

On error, the members are `"status"`, which will map to the literal string `"error"`, and `"error"`, which will map to a string description of the error.

The provided examples reflect this schema.

## Types

### `datasource`

A `datasource` is either an object with the single field `model` with :

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| model             | string                   | Filename to use as a data source                                  |

Example:

```JSON
{ "model": "model.gromet" }
```

Or a string containing the data itself:

```JSON
"model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n  let s_initial = 997..."
```

### `model-def`

A model def is a `datasource` along with a model type.  Valid model types are `easel`, `diff-eqs`, `core`, `gromet-pnc`, and `gromet-prt`.

Example:

```
{
  "source": { "model": "sir.easel" },
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


| Field   | Type    | Description                                                       |
|---------|---------|-------------------------------------------------------------------|
| models  | list    | list of "model-def++" objects - guaranteed to have the same members as a model-def object, but will also include `"name"` and `"description"` members for ESL models |

Example:

```JSON
{
  "status": "success",
  "result": {
    "models": [
      {
        "source": { "model": "sir.easel" },
        "type": "easel",
        "name": "SIR",
        "description": "No description."
      }
    ]
  }
}
```

### `simulate` - simulate a model

**Request:**

| Field            | Type                     | Description                                                         | Stipulations
|------------------|--------------------------|---------------------------------------------------------------------|----------------
| command          | string                   | The literal `"simulate"`                                            |
| sim-type         | string                   | (optional) Simulation engine - one of `"aj"`, `"discrete"`, `"gsl"` | Defaults to `gsl`
| definition       | model-def                | Definition of the model                                             | 
| start            | number                   | Start time of the simulation                                        | 
| end              | number                   | End time of the simulation                                          | 
| step             | number                   | Simulation time step size                                           | 
| seed             | integer                  | (optional) Seed for random number generation/event selection        | Only valid when `sim-type` is `discrete`
| domain-parameter | string                   | (optional) Independent variable over which start/step/end varies    | Not yet supported, and only valid when simulating function networks
| parameters       | dict                     | (optional) Parameter values/initial conditions for the simulation   | When `sim-type` is `aj`, parameters specified neither here nor in the model will default to 0
| outputs          | list                     | Restrict model output to these values                               | Leave empty to output all variables

Example:

```JSON
{
  "command": "simulate",
  "sim-type": "gsl",
  "definition": {
    "type": "easel",
    "source": { "model": "sir.easel" }
  },
  "start": 0,
  "end": 120.0,
  "step": 30.0,
  "parameters": {
    "beta": 0.9
  }
}
```

**Response:**

| Field            | Type                             | Description                                                       |
|------------------|----------------------------------|-------------------------------------------------------------------|
| times            | list of number                   | series of times used in simulation                                |
| values           | list of result series object     | values of state varaibles                                         |

Each object in `result` is structured such that each key is the name of a model variable `V` and each value is a list `l` such that `V` has the value `l[x]` at time `times[x]`.

Example:

```JSON
{
  "status": "success",
  "result": {
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
    "source": { "model": "sir.easel" }
  }
}
```

**Response:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| nodes            | list of node             | A node has a `name` and a `type`                                  |
| edges            | list of edge             | An edge is a "tuple" mapping one node to another                  |


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
    "source": { "model": "sir.easel" }
  }
}
```

**Response:**

The result, if successful, is a `model-def` object with the source inline.

```JSON
{
  "status": "success",
  "result": {
    "source": "model SIR:\n  let beta = 0.4\n  let gamma = 0.04\n\n  let s_initial = 997\n  let i_initial = 3\n  let r_initial = 0\n\n  state S = s_initial\n  state I = i_initial\n  state R = r_initial\n\n  let total_population = S + I + R\n\n  event Infect:\n    when:\n      S > 0 and I > 0\n    rate: \n      beta * S * I / total_population\n    effect:\n      S -= 1\n      I += 1\n      \n  event Remove:\n    when:\n      I > 0\n    rate: \n      gamma * I\n    effect:\n      I -= 1\n      R += 1\n      \n      ",
    "type": "easel"
  }
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
    "source": { "model": "sir.easel" }
  },
  "dest-type":"diff-eqs"
}
```

**Response:**

The result, if successful, is a `model-def` object with the new model inline.

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
      "model": "sir.easel"
    }
  }
}
```

**Response**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| measures         | list of state variables  | Something the can be measured                                     |
| parameters       | list of parameter        | Something that can be tweaked                                     |

Both `measures` and `parameters` are lists of objects, where each object has
`uid`, `value_type` and `metadata` fields. In addition, parameters may have
a `default` field.

The `metadata` is an object with variable fields, but some of interest
are `description`, `name`, and `group`.  In particular, `group` may be used
as a hint to group related parameters.


**Response:**

```JSON
{
  "status": "success",
  "result": {
    "measures": [
      {
        "metadata": {
          "name": "I"
        },
        "value_type": "Real",
        "uid": "I"
      },
      {
        "metadata": {
          "name": "R"
        },
        "value_type": "Real",
        "uid": "R"
      },
      {
        "metadata": {
          "name": "S"
        },
        "value_type": "Real",
        "uid": "S"
      },
      {
        "metadata": {
          "name": "total_population"
        },
        "value_type": "Real",
        "uid": "total_population"
      }
    ],
    "parameters": [
      {
        "metadata": {
          "name": "beta"
        },
        "value_type": "Real",
        "uid": "beta"
      },
      {
        "metadata": {
          "name": "gamma"
        },
        "value_type": "Real",
        "uid": "gamma"
      },
      {
        "metadata": {
          "name": "i_initial"
        },
        "value_type": "Real",
        "uid": "i_initial"
      },
      {
        "metadata": {
          "name": "r_initial"
        },
        "value_type": "Real",
        "uid": "r_initial"
      },
      {
        "metadata": {
          "name": "s_initial"
        },
        "value_type": "Real",
        "uid": "s_initial"
      },
      {
        "metadata": {
          "name": "beta"
        },
        "value_type": "Real",
        "default": 0.4,
        "uid": "beta"
      },
      {
        "metadata": {
          "name": "gamma"
        },
        "value_type": "Real",
        "default": 0.04,
        "uid": "gamma"
      },
      {
        "metadata": {
          "name": "i_initial"
        },
        "value_type": "Real",
        "default": 3,
        "uid": "i_initial"
      },
      {
        "metadata": {
          "name": "r_initial"
        },
        "value_type": "Real",
        "default": 0,
        "uid": "r_initial"
      },
      {
        "metadata": {
          "name": "s_initial"
        },
        "value_type": "Real",
        "default": 997,
        "uid": "s_initial"
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

The result, if successful, is a `model-def` object with the new file in the `source` field.

Example:
```JSON
{
  "status": "success",
  "result": {
    "source": { "model": "sir.easel" },
    "type": "easel"
  }
}
```
