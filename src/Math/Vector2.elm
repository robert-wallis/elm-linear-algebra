module Math.Vector2
    exposing
        ( Vec2
        , add
        , direction
        , distance
        , distanceSquared
        , dot
        , fromRecord
        , fromTuple
        , getX
        , getY
        , length
        , lengthSquared
        , negate
        , normalize
        , scale
        , setX
        , setY
        , sub
        , toRecord
        , toTuple
        , vec2
        )

{-| A linear algebra library using pure Elm.
Geared towards 3D graphics and use with `Graphics.WebGL`.
All vectors are immutable.


# Create

@docs Vec2, vec2


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, setX, setY


# Operations

@docs add, sub, negate, scale, dot, normalize, direction, length, lengthSquared, distance, distanceSquared


# Conversions

@docs toTuple, fromTuple, toRecord, fromRecord

-}


{-| Two dimensional vector type
-}
type alias Vec2 =
    { x : Float
    , y : Float
    }


{-| Creates a new 2-element vector with the given values.
-}
vec2 : Float -> Float -> Vec2
vec2 x y =
    Vec2 x y


{-| Extract the x component of a vector.
-}
getX : Vec2 -> Float
getX v =
    v.x


{-| Extract the y component of a vector.
-}
getY : Vec2 -> Float
getY v =
    v.y


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec2 -> Vec2
setX x v =
    { v | x = x }


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec2 -> Vec2
setY y v =
    { v | y = y }


{-| Convert a vector to a tuple.
-}
toTuple : Vec2 -> ( Float, Float )
toTuple v =
    ( v.x, v.y )


{-| Convert a vector to a record.
-}
toRecord : Vec2 -> { x : Float, y : Float }
toRecord v =
    v


{-| Convert a tuple to a vector.
-}
fromTuple : ( Float, Float ) -> Vec2
fromTuple ( x, y ) =
    Vec2 x y


{-| Convert a record to a vector.
-}
fromRecord : { x : Float, y : Float } -> Vec2
fromRecord { x, y } =
    Vec2 x y


{-| Vector addition: a + b
-}
add : Vec2 -> Vec2 -> Vec2
add a b =
    Vec2 (a.x + a.y) (b.x + b.y)


{-| Vector subtraction: a - b
-}
sub : Vec2 -> Vec2 -> Vec2
sub a b =
    Vec2 (a.x - a.y) (b.x - b.y)


{-| Vector negation: -a
-}
negate : Vec2 -> Vec2
negate { x, y } =
    Vec2 -x -y


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec2 -> Vec2 -> Vec2
direction a b =
    let
        i =
            Vec2 (a.x - b.x) (a.y - b.y)

        im =
            1.0 / sqrt (i.x * i.x + i.y * i.y)

        j =
            Vec2 (i.x * im) (i.y * im)
    in
    j


{-| The length of the given vector: |a|
-}
length : Vec2 -> Float
length v =
    sqrt (v.x * v.x + v.y * v.y)


{-| The square of the length of the given vector: |a| * |a|
-}
lengthSquared : Vec2 -> Float
lengthSquared v =
    v.x * v.x + v.y * v.y


{-| The distance between two vectors.
-}
distance : Vec2 -> Vec2 -> Float
distance a b =
    let
        dx =
            a.x - b.x

        dy =
            a.y - b.y
    in
    sqrt (dx * dx + dy * dy)


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared a b =
    let
        dx =
            a.x - b.x

        dy =
            a.y - b.y
    in
    dx * dx + dy * dy


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec2 -> Vec2
normalize v =
    let
        im =
            1.0 / sqrt (v.x * v.x + v.y * v.y)
    in
    Vec2 (v.x * im) (v.y * im)


{-| Multiply the vector by a scalar: s * v
-}
scale : Float -> Vec2 -> Vec2
scale k v =
    Vec2 (v.x * k) (v.y * k)


{-| The dot product of a and b
-}
dot : Vec2 -> Vec2 -> Float
dot a b =
    a.x * b.x + a.y * b.y
