# This document specifies the nature of casting and promotion for Micro C

## During a binary operation types will be promoted to the type of the highest promotion scoring

out of the two types, described in the below table.

        Ex:  char * long = long

#### Binary Type Promotion Scoring

| type   | promotion score | 
             |:-------|:----------------|
| char   | 1               |
| int    | 2               |
| float  | 3               | 
| long   | 4               |
| double | 5               |

## Any operation between a signed type and an unsigned type will result in an unsigned type.

        Ex: int * unsigned long = long

## Implicit Casting:

#### Conversions made implicitly

| type             | into    |
|:-----------------|:--------|
| numeric          | numeric |
| T*               | void*   |
| void*            | T*      |
| array&#60;T&#62; | T*      |

## Explicit Casting:

#### Conversions that can be made explicitly

- All implicit casts

| type             | into |
|:-----------------|:-----|
| any*             | any* |
| integer          | any* |
| any*             | long |
| array&#60;T&#62; | any* |
 
    