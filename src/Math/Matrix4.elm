module Math.Matrix4
    exposing
        ( Mat4
        , fromRecord
        , identity
        , inverse
        , inverseOrthonormal
        , makeBasis
        , makeFromList
        , makeFrustum
        , makeLookAt
        , makeOrtho
        , makeOrtho2D
        , makePerspective
        , makeRotate
        , makeScale
        , makeScale3
        , makeTranslate
        , makeTranslate3
        , mul
        , mulAffine
        , rotate
        , scale
        , scale3
        , toRecord
        , transform
        , translate
        , translate3
        , transpose
        )

{-| A linear algebra library using pure Elm.
Geared towards 3D graphics and use with `Graphics.WebGL`.
All matrices are immutable.


# Create

@docs Mat4, identity, makeFromList


# Operations

@docs inverse, inverseOrthonormal, mul, mulAffine, transpose, makeBasis, transform


# Projections

@docs makeFrustum, makePerspective, makeOrtho, makeOrtho2D, makeLookAt


# Apply Transformations

@docs rotate, scale, scale3, translate, translate3


# Create Transformations

@docs makeRotate, makeScale, makeScale3, makeTranslate, makeTranslate3


# Conversions

@docs toRecord, fromRecord

-}

import Math.Vector3 exposing (Vec3, dot)
import Native.MJS


{-| 4x4 matrix type
-}
type alias Mat4 =
    { m11 : Float
    , m21 : Float
    , m31 : Float
    , m41 : Float
    , m12 : Float
    , m22 : Float
    , m32 : Float
    , m42 : Float
    , m13 : Float
    , m23 : Float
    , m33 : Float
    , m43 : Float
    , m14 : Float
    , m24 : Float
    , m34 : Float
    , m44 : Float
    }


{-| Multiply a vector by a 4x4 matrix: m * v
-}
transform : Mat4 -> Vec3 -> Vec3
transform =
    Native.MJS.v3mul4x4


{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Mat4
identity =
    Mat4 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1


{-| Computes the inverse of any matrix. This is somewhat computationally
intensive. If the matrix is not invertible, `Nothing` is returned.
-}
inverse : Mat4 -> Maybe Mat4
inverse { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    let
        -- https://stackoverflow.com/questions/1148309/inverting-a-4x4-matrix
        inv =
            Mat4
                (m22 * m33 * m44 - m22 * m43 * m34 - m23 * m32 * m44 + m23 * m42 * m34 + m24 * m32 * m43 - m24 * m42 * m33)
                (-m21 * m33 * m44 + m21 * m43 * m34 + m23 * m31 * m44 - m23 * m41 * m34 - m24 * m31 * m43 + m24 * m41 * m33)
                (m21 * m32 * m44 - m21 * m42 * m34 - m22 * m31 * m44 + m22 * m41 * m34 + m24 * m31 * m42 - m24 * m41 * m32)
                (-m21 * m32 * m43 + m21 * m42 * m33 + m22 * m31 * m43 - m22 * m41 * m33 - m23 * m31 * m42 + m23 * m41 * m32)
                (-m12 * m33 * m44 + m12 * m43 * m34 + m13 * m32 * m44 - m13 * m42 * m34 - m14 * m32 * m43 + m14 * m42 * m33)
                (m11 * m33 * m44 - m11 * m43 * m34 - m13 * m31 * m44 + m13 * m41 * m34 + m14 * m31 * m43 - m14 * m41 * m33)
                (-m11 * m32 * m44 + m11 * m42 * m34 + m12 * m31 * m44 - m12 * m41 * m34 - m14 * m31 * m42 + m14 * m41 * m32)
                (m11 * m32 * m43 - m11 * m42 * m33 - m12 * m31 * m43 + m12 * m41 * m33 + m13 * m31 * m42 - m13 * m41 * m32)
                (m12 * m23 * m44 - m12 * m43 * m24 - m13 * m22 * m44 + m13 * m42 * m24 + m14 * m22 * m43 - m14 * m42 * m23)
                (-m11 * m23 * m44 + m11 * m43 * m24 + m13 * m21 * m44 - m13 * m41 * m24 - m14 * m21 * m43 + m14 * m41 * m23)
                (m11 * m22 * m44 - m11 * m42 * m24 - m12 * m21 * m44 + m12 * m41 * m24 + m14 * m21 * m42 - m14 * m41 * m22)
                (-m11 * m22 * m43 + m11 * m42 * m23 + m12 * m21 * m43 - m12 * m41 * m23 - m13 * m21 * m42 + m13 * m41 * m22)
                (-m12 * m23 * m34 + m12 * m33 * m24 + m13 * m22 * m34 - m13 * m32 * m24 - m14 * m22 * m33 + m14 * m32 * m23)
                (m11 * m23 * m34 - m11 * m33 * m24 - m13 * m21 * m34 + m13 * m31 * m24 + m14 * m21 * m33 - m14 * m31 * m23)
                (-m11 * m22 * m34 + m11 * m32 * m24 + m12 * m21 * m34 - m12 * m31 * m24 - m14 * m21 * m32 + m14 * m31 * m22)
                (m11 * m22 * m33 - m11 * m32 * m23 - m12 * m21 * m33 + m12 * m31 * m23 + m13 * m21 * m32 - m13 * m31 * m22)

        det =
            m11 * inv.m11 + m21 * inv.m12 + m31 * inv.m13 + m41 * inv.m14
    in
    case det of
        0 ->
            Nothing

        _ ->
            let
                d =
                    1 / det
            in
            Mat4
                (inv.m11 * d)
                (inv.m21 * d)
                (inv.m31 * d)
                (inv.m41 * d)
                (inv.m12 * d)
                (inv.m22 * d)
                (inv.m32 * d)
                (inv.m42 * d)
                (inv.m13 * d)
                (inv.m23 * d)
                (inv.m33 * d)
                (inv.m43 * d)
                (inv.m14 * d)
                (inv.m24 * d)
                (inv.m34 * d)
                (inv.m44 * d)


{-| Computes the inverse of the given matrix, assuming that the matrix is
orthonormal. This algorithm is more efficient than general matrix inversion, and
has no possibility of failing.
-}
inverseOrthonormal : Mat4 -> Mat4
inverseOrthonormal m =
    let
        r =
            transpose m

        t =
            Vec3 m.m41 m.m42 m.m43
    in
    { r
        | m14 = 0
        , m24 = 0
        , m34 = 0
        , m41 = -(dot (Vec3 r.m11 r.m21 r.m31) t)
        , m42 = -(dot (Vec3 m.m12 m.m22 m.m32) t)
        , m43 = -(dot (Vec3 m.m13 m.m23 m.m33) t)
    }


{-| Creates a matrix for a projection frustum with the given parameters.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum
  - znear - the near z distance of the frustum
  - zfar - the far z distance of the frustum

-}
makeFrustum : Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeFrustum =
    Native.MJS.m4x4makeFrustum


{-| Creates a matrix for a perspective projection with the given parameters.

Parameters:

  - fovy - field of view in the y axis, in degrees
  - aspect - aspect ratio
  - znear - the near z distance of the projection
  - zfar - the far z distance of the projection

-}
makePerspective : Float -> Float -> Float -> Float -> Mat4
makePerspective =
    Native.MJS.m4x4makePerspective


{-| Creates a matrix for an orthogonal frustum projection with the given parameters.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum
  - znear - the near z distance of the frustum
  - zfar - the far z distance of the frustum

-}
makeOrtho : Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeOrtho =
    Native.MJS.m4x4makeOrtho


{-| Creates a matrix for a 2D orthogonal frustum projection with the given
parameters. `znear` and `zfar` are assumed to be -1 and 1, respectively.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum

-}
makeOrtho2D : Float -> Float -> Float -> Float -> Mat4
makeOrtho2D =
    Native.MJS.m4x4makeOrtho2D


{-| Matrix multiplcation: a * b
-}
mul : Mat4 -> Mat4 -> Mat4
mul =
    Native.MJS.m4x4mul


{-| Matrix multiplication, assuming a and b are affine: a * b
-}
mulAffine : Mat4 -> Mat4 -> Mat4
mulAffine =
    Native.MJS.m4x4mulAffine


{-| Creates a transformation matrix for rotation in radians about the
3-element vector axis.
-}
makeRotate : Float -> Vec3 -> Mat4
makeRotate =
    Native.MJS.m4x4makeRotate


{-| Concatenates a rotation in radians about an axis to the given matrix.
-}
rotate : Float -> Vec3 -> Mat4 -> Mat4
rotate =
    Native.MJS.m4x4rotate


{-| Creates a transformation matrix for scaling by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeScale3 : Float -> Float -> Float -> Mat4
makeScale3 =
    Native.MJS.m4x4makeScale3


{-| Creates a transformation matrix for scaling each of the x, y, and z axes by
the amount given in the corresponding element of the 3-element vector.
-}
makeScale : Vec3 -> Mat4
makeScale =
    Native.MJS.m4x4makeScale


{-| Concatenates a scaling to the given matrix.
-}
scale3 : Float -> Float -> Float -> Mat4 -> Mat4
scale3 =
    Native.MJS.m4x4scale3


{-| Concatenates a scaling to the given matrix.
-}
scale : Vec3 -> Mat4 -> Mat4
scale =
    Native.MJS.m4x4scale


{-| Creates a transformation matrix for translating by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeTranslate3 : Float -> Float -> Float -> Mat4
makeTranslate3 =
    Native.MJS.m4x4makeTranslate3


{-| Creates a transformation matrix for translating each of the x, y, and z
axes by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate : Vec3 -> Mat4
makeTranslate =
    Native.MJS.m4x4makeTranslate


{-| Concatenates a translation to the given matrix.
-}
translate3 : Float -> Float -> Float -> Mat4 -> Mat4
translate3 =
    Native.MJS.m4x4translate3


{-| Concatenates a translation to the given matrix.
-}
translate : Vec3 -> Mat4 -> Mat4
translate =
    Native.MJS.m4x4translate


{-| Creates a transformation matrix for a camera.

Parameters:

  - eye - The location of the camera
  - center - The location of the focused object
  - up - The "up" direction according to the camera

-}
makeLookAt : Vec3 -> Vec3 -> Vec3 -> Mat4
makeLookAt =
    Native.MJS.m4x4makeLookAt


{-| "Flip" the matrix across the diagonal by swapping row index and column
index.
-}
transpose : Mat4 -> Mat4
transpose { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    Mat4 m11 m12 m13 m14 m21 m22 m23 m24 m31 m32 m33 m34 m41 m42 m43 m44


{-| Creates a transform from a basis consisting of 3 linearly independent vectors.
-}
makeBasis : Vec3 -> Vec3 -> Vec3 -> Mat4
makeBasis =
    Native.MJS.m4x4makeBasis


{-| Creates a matrix from a list of elements. Returns Nothing if the length of
the list is not exactly 16 (4x4).
-}
makeFromList : List Float -> Maybe Mat4
makeFromList =
    Native.MJS.m4x4fromList


{-| Convert a matrix to a record.
-}
toRecord : Mat4 -> { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float }
toRecord =
    Native.MJS.m4x4toRecord


{-| Convert a record to a matrix.
-}
fromRecord : { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float } -> Mat4
fromRecord =
    Native.MJS.m4x4fromRecord
