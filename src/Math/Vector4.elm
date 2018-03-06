module Math.Vector4
    exposing
        ( Vec4
        , add
        , direction
        , distance
        , distanceSquared
        , dot
        , fromRecord
        , fromTuple
        , getW
        , getX
        , getY
        , getZ
        , length
        , lengthSquared
        , negate
        , normalize
        , scale
        , setW
        , setX
        , setY
        , setZ
        , sub
        , toRecord
        , toTuple
        , vec4
        )

{-| A linear algebra library using pure Elm.
Geared towards 3D graphics and use with `Graphics.WebGL`.
All vectors are immutable.


# Create

@docs Vec4, vec4


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, getW, setX, setY, setZ, setW


# Operations

@docs add, sub, negate, scale, dot, normalize, direction, length, lengthSquared, distance, distanceSquared


# Conversions

@docs toTuple, fromTuple, toRecord, fromRecord

-}


{-| Four dimensional vector type
-}
type alias Vec4 =
    { x : Float
    , y : Float
    , z : Float
    , w : Float
    }


{-| Creates a new 4-element vector with the given x, y, z, and w values.
-}
vec4 : Float -> Float -> Float -> Float -> Vec4
vec4 x y z w =
    Vec4 x y z w


{-| Extract the x component of a vector.
-}
getX : Vec4 -> Float
getX v =
    v.x


{-| Extract the y component of a vector.
-}
getY : Vec4 -> Float
getY v =
    v.y


{-| Extract the z component of a vector.
-}
getZ : Vec4 -> Float
getZ v =
    v.z


{-| Extract the w component of a vector.
-}
getW : Vec4 -> Float
getW v =
    v.w


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec4 -> Vec4
setX x v =
    { v | x = x }


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec4 -> Vec4
setY y v =
    { v | y = y }


{-| Update the z component of a vector, returning a new vector.
-}
setZ : Float -> Vec4 -> Vec4
setZ z v =
    { v | z = z }


{-| Update the w component of a vector, returning a new vector.
-}
setW : Float -> Vec4 -> Vec4
setW w v =
    { v | w = w }


{-| Convert a vector to a tuple.
-}
toTuple : Vec4 -> ( Float, Float, Float, Float )
toTuple { x, y, z, w } =
    ( x, y, z, w )


{-| Convert a vector to a record.
-}
toRecord : Vec4 -> { x : Float, y : Float, z : Float, w : Float }
toRecord { x, y, z, w } =
    Vec4 x y z w


{-| Convert a tuple to a vector.
-}
fromTuple : ( Float, Float, Float, Float ) -> Vec4
fromTuple ( x, y, z, w ) =
    Vec4 x y z w


{-| Convert a record to a vector.
-}
fromRecord : { x : Float, y : Float, z : Float, w : Float } -> Vec4
fromRecord { x, y, z, w } =
    Vec4 x y z w


{-| Vector addition: a + b
-}
add : Vec4 -> Vec4 -> Vec4
add a b =
    Vec4
        (a.x + b.x)
        (a.y + b.y)
        (a.z + b.z)
        (a.w + b.w)


{-| Vector subtraction: a - b
-}
sub : Vec4 -> Vec4 -> Vec4
sub a b =
    Vec4
        (a.x - b.x)
        (a.y - b.y)
        (a.z - b.z)
        (a.w - b.w)


{-| Vector negation: -a
-}
negate : Vec4 -> Vec4
negate { x, y, z, w } =
    Vec4 -x -y -z -w


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec4 -> Vec4 -> Vec4
direction a b =
    let
        ( x, y, z, w ) =
            ( a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w )

        im =
            sqrt (x * x + y * y + z * z + w * w)
    in
    Vec4 (a.x * im) (a.y * im) (a.z * im) (a.w * im)


{-| The length of the given vector: |a|
-}
length : Vec4 -> Float
length { x, y, z, w } =
    sqrt (x * x + y * y + z * z + w * w)


{-| The square of the length of the given vector: |a| * |a|
-}
lengthSquared : Vec4 -> Float
lengthSquared { x, y, z, w } =
    x * x + y * y + z * z + w * w


{-| The distance between two vectors.
-}
distance : Vec4 -> Vec4 -> Float
distance a b =
    let
        ( x, y, z, w ) =
            ( a.x - b.z, a.y - b.y, a.z - b.z, a.w - b.w )
    in
    sqrt (x * x + y * y + z * z + w * w)


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec4 -> Vec4 -> Float
distanceSquared a b =
    let
        ( x, y, z, w ) =
            ( a.x - b.z, a.y - b.y, a.z - b.z, a.w - b.w )
    in
    x * x + y * y + z * z + w * w


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec4 -> Vec4
normalize { x, y, z, w } =
    let
        im =
            1.0 / sqrt (x * x + y * y + z * z + w * w)
    in
    Vec4 (x * im) (y * im) (z * im) (w * im)


{-| Multiply the vector by a scalar: s * v
-}
scale : Float -> Vec4 -> Vec4
scale s { x, y, z, w } =
    Vec4 (s * x) (s * y) (s * z) (s * w)


{-| The dot product of a and b
-}
dot : Vec4 -> Vec4 -> Float
dot a b =
    a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
