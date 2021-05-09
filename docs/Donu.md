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
| models           | list of string pairs     | list of model filenames and model type pairs                      |

Example:

```JSON
{ "models": [["SIR.easel", "easel"],
             ["CHIME.gromet", "gromet"]]}
```

### `simulate` - Simulate a model using ODEs

**Request:**

| Field            | Type                     | Description                                                       |
|------------------|--------------------------|-------------------------------------------------------------------|
| command          | string                   | Command - for this operation it will be the string `"simulate"`   |
| model            | string                   | Type of model to simulate, valid options are:  `"easel"`, `"gromet"` (coming soon!) |
| definition       | datasource               | Reference to the implementation of the model                      |
| start            | number                   | Start time of the simulation                                      |
| end              | number                   | End time of the simulation                                        |
| step             | number                   | Simulation time step size                                         |

Example:

```JSON
{
  "command": "simulate",
  "model": "easel",
  "definition": "SIR.easel",
  "start": 0,
  "end": 120.0,
  "step": 1.0
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
  "times": [0.0, 30.0, 60.0, 90.0, 120.0],
  "values": {
    "S": [997.0,  700.0, 300.0, 100.0, 50.0 ],
    "I": [3.0,    200.0, 50.0,  20.0,  10.0 ],
    "R": [0.0,    100.0, 650.0, 880.0, 940.0]
  }
}
```
