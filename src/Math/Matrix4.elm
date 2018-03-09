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

import Math.Vector3 exposing (Vec3, dot, normalize)
import Maybe
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


{-| Multiply a 3D vector by a 4x4 matrix: m * v
The 4 element result vector is divided by the w component to make a 3 element result.
-}
transform : Mat4 -> Vec3 -> Vec3
transform { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } v =
    let
        w =
            dot v (Vec3 m14 m24 m34) + m44
    in
    Vec3
        ((dot v (Vec3 m11 m21 m31) + m41) / w)
        ((dot v (Vec3 m12 m22 m32) + m42) / w)
        ((dot v (Vec3 m13 m23 m33) + m43) / w)


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
            Just
                (Mat4
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
                )


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
makeFrustum left right bottom top zNear zFar =
    Mat4
        (2 * zNear / (right - left))
        0
        0
        0
        0
        (2 * zNear / (top - bottom))
        0
        0
        ((right + left) / (right - left))
        ((top + bottom) / (top - bottom))
        (-(zFar + zNear) / (zFar - zNear))
        -1
        0
        0
        (-2 * zFar * zNear / (zFar - zNear))
        0


{-| Creates a matrix for a perspective projection with the given parameters.

Parameters:

  - fovy - field of view in the y axis, in degrees
  - aspect - aspect ratio
  - znear - the near z distance of the projection
  - zfar - the far z distance of the projection

-}
makePerspective : Float -> Float -> Float -> Float -> Mat4
makePerspective fovY aspect zNear zFar =
    let
        yMin =
            zNear * (tan fovY * pi / 360.0)

        yMax =
            -yMin

        xMin =
            yMax * aspect

        xMax =
            yMin * aspect
    in
    makeFrustum xMin xMax yMax yMin zNear zFar


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
makeOrtho left right bottom top zNear zFar =
    Mat4
        (2 / (right - left))
        0
        0
        0
        0
        (2 / (top - bottom))
        0
        0
        0
        0
        -(2 / (zFar - zNear))
        0
        -((right + left) / (right - left))
        -((top + bottom) / (top - bottom))
        -((zFar + zNear) / (zFar - zNear))
        1


{-| Creates a matrix for a 2D orthogonal frustum projection with the given
parameters. `znear` and `zfar` are assumed to be -1 and 1, respectively.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum

-}
makeOrtho2D : Float -> Float -> Float -> Float -> Mat4
makeOrtho2D left right bottom top =
    makeOrtho left right bottom top -1 1


{-| Matrix multiplication: a * b
-}
mul : Mat4 -> Mat4 -> Mat4
mul a b =
    Mat4
        (a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31 + a.m14 * b.m41)
        (a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31 + a.m24 * b.m41)
        (a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31 + a.m34 * b.m41)
        (a.m41 * b.m11 + a.m42 * b.m21 + a.m43 * b.m31 + a.m44 * b.m41)
        (a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32 + a.m14 * b.m42)
        (a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32 + a.m24 * b.m42)
        (a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32 + a.m34 * b.m42)
        (a.m41 * b.m12 + a.m42 * b.m22 + a.m43 * b.m32 + a.m44 * b.m42)
        (a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33 + a.m14 * b.m43)
        (a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33 + a.m24 * b.m43)
        (a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33 + a.m34 * b.m43)
        (a.m41 * b.m13 + a.m42 * b.m23 + a.m43 * b.m33 + a.m44 * b.m43)
        (a.m11 * b.m14 + a.m12 * b.m24 + a.m13 * b.m34 + a.m14 * b.m44)
        (a.m21 * b.m14 + a.m22 * b.m24 + a.m23 * b.m34 + a.m24 * b.m44)
        (a.m31 * b.m14 + a.m32 * b.m24 + a.m33 * b.m34 + a.m34 * b.m44)
        (a.m41 * b.m14 + a.m42 * b.m24 + a.m43 * b.m34 + a.m44 * b.m44)


{-| Matrix multiplication, assuming a and b are affine: a * b
-}
mulAffine : Mat4 -> Mat4 -> Mat4
mulAffine a b =
    Mat4
        (a.m11 * b.m11 + a.m12 * b.m21 + a.m13 * b.m31)
        (a.m21 * b.m11 + a.m22 * b.m21 + a.m23 * b.m31)
        (a.m31 * b.m11 + a.m32 * b.m21 + a.m33 * b.m31)
        0
        (a.m11 * b.m12 + a.m12 * b.m22 + a.m13 * b.m32)
        (a.m21 * b.m12 + a.m22 * b.m22 + a.m23 * b.m32)
        (a.m31 * b.m12 + a.m32 * b.m22 + a.m33 * b.m32)
        0
        (a.m11 * b.m13 + a.m12 * b.m23 + a.m13 * b.m33)
        (a.m21 * b.m13 + a.m22 * b.m23 + a.m23 * b.m33)
        (a.m31 * b.m13 + a.m32 * b.m23 + a.m33 * b.m33)
        0
        (a.m11 * b.m14 + a.m12 * b.m24 + a.m13 * b.m34 + a.m14)
        (a.m21 * b.m14 + a.m22 * b.m24 + a.m23 * b.m34 + a.m24)
        (a.m31 * b.m14 + a.m32 * b.m24 + a.m33 * b.m34 + a.m34)
        1


{-| Creates a transformation matrix for rotation in radians about the
3-element vector axis.
-}
makeRotate : Float -> Vec3 -> Mat4
makeRotate angle axis =
    let
        { x, y, z } =
            normalize axis

        c =
            cos angle

        c1 =
            1 - c

        s =
            sin angle
    in
    Mat4
        (x * x * c1 + c)
        (y * x * c1 + z * s)
        (z * x * c1 - y * s)
        0
        (x * y * c1 - z * s)
        (y * y * c1 + c)
        (y * z * c1 + x * s)
        0
        (x * z * c1 + y * s)
        (y * z * c1 - x * s)
        (z * z * c1 + c)
        0
        0
        0
        0
        1


{-| Concatenates a rotation in radians about an axis to the given matrix.
-}
rotate : Float -> Vec3 -> Mat4 -> Mat4
rotate angle axis { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    let
        c =
            cos angle

        c1 =
            1 - c

        s =
            sin angle

        { x, y, z } =
            normalize axis

        xs =
            x * s

        ys =
            y * s

        zs =
            z * s

        xyc1 =
            x * y * c1

        xzc1 =
            x * z * c1

        yzc1 =
            y * z * c1

        t11 =
            x * x * c1 + c

        t21 =
            xyc1 + zs

        t31 =
            xzc1 - ys

        t12 =
            xyc1 - zs

        t22 =
            y * y * c1 + c

        t32 =
            yzc1 + xs

        t13 =
            xzc1 + ys

        t23 =
            yzc1 - xs

        t33 =
            z * z * c1 + c
    in
    Mat4
        (m11 * t11 + m12 * t21 + m13 * t31)
        (m21 * t11 + m22 * t21 + m23 * t31)
        (m31 * t11 + m32 * t21 + m33 * t31)
        (m41 * t11 + m42 * t21 + m43 * t31)
        (m11 * t12 + m12 * t22 + m13 * t32)
        (m21 * t12 + m22 * t22 + m23 * t32)
        (m31 * t12 + m32 * t22 + m33 * t32)
        (m41 * t12 + m42 * t22 + m43 * t32)
        (m11 * t13 + m12 * t23 + m13 * t33)
        (m21 * t13 + m22 * t23 + m23 * t33)
        (m31 * t13 + m32 * t23 + m33 * t33)
        (m41 * t13 + m42 * t23 + m43 * t33)
        m14
        m24
        m34
        m44


{-| Creates a transformation matrix for scaling by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeScale3 : Float -> Float -> Float -> Mat4
makeScale3 x y z =
    Mat4 x 0 0 0 0 y 0 0 0 0 z 0 0 0 0 1


{-| Creates a transformation matrix for scaling each of the x, y, and z axes by
the amount given in the corresponding element of the 3-element vector.
-}
makeScale : Vec3 -> Mat4
makeScale { x, y, z } =
    Mat4 x 0 0 0 0 y 0 0 0 0 z 0 0 0 0 1


{-| Concatenates a scaling to the given matrix.
-}
scale3 : Float -> Float -> Float -> Mat4 -> Mat4
scale3 x y z { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    Mat4
        (m11 * x)
        (m21 * x)
        (m31 * x)
        (m41 * x)
        (m12 * y)
        (m22 * y)
        (m32 * y)
        (m42 * y)
        (m13 * z)
        (m23 * z)
        (m33 * z)
        (m43 * z)
        m14
        m24
        m34
        m44


{-| Concatenates a scaling to the given matrix.
-}
scale : Vec3 -> Mat4 -> Mat4
scale { x, y, z } { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    Mat4
        (m11 * x)
        (m21 * x)
        (m31 * x)
        (m41 * x)
        (m12 * y)
        (m22 * y)
        (m32 * y)
        (m42 * y)
        (m13 * z)
        (m23 * z)
        (m33 * z)
        (m43 * z)
        m14
        m24
        m34
        m44


{-| Creates a transformation matrix for translating by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeTranslate3 : Float -> Float -> Float -> Mat4
makeTranslate3 x y z =
    Mat4 1 0 0 0 0 1 0 0 0 0 1 0 x y z 1


{-| Creates a transformation matrix for translating each of the x, y, and z
axes by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate : Vec3 -> Mat4
makeTranslate { x, y, z } =
    Mat4 1 0 0 0 0 1 0 0 0 0 1 0 x y z 1


{-| Concatenates a translation to the given matrix.
-}
translate3 : Float -> Float -> Float -> Mat4 -> Mat4
translate3 x y z { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    Mat4
        m11
        m21
        m31
        m41
        m12
        m22
        m32
        m42
        m13
        m23
        m33
        m43
        (m11 * x + m12 * y + m13 * z + m14)
        (m21 * x + m22 * y + m23 * z + m24)
        (m31 * x + m32 * y + m33 * z + m34)
        (m41 * x + m42 * y + m43 * z + m44)


{-| Concatenates a translation to the given matrix.
-}
translate : Vec3 -> Mat4 -> Mat4
translate { x, y, z } { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    Mat4
        m11
        m21
        m31
        m41
        m12
        m22
        m32
        m42
        m13
        m23
        m33
        m43
        (m11 * x + m12 * y + m13 * z + m14)
        (m21 * x + m22 * y + m23 * z + m24)
        (m31 * x + m32 * y + m33 * z + m34)
        (m41 * x + m42 * y + m43 * z + m44)


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
makeBasis x y z =
    Mat4 x.x x.y x.z 0 y.x y.y y.z 0 z.x z.y z.z 0 0 0 0 1


{-| Creates a matrix from a list of elements. Returns Nothing if the length of
the list is not exactly 16 (4x4).
-}
makeFromList : List Float -> Maybe Mat4
makeFromList list =
    case list of
        [ m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 ] ->
            Just (Mat4 m11 m21 m31 m41 m12 m22 m32 m42 m13 m23 m33 m43 m14 m24 m34 m44)

        _ ->
            Nothing


{-| Convert a matrix to a record.
-}
toRecord : Mat4 -> { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float }
toRecord { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    Mat4 m11 m21 m31 m41 m12 m22 m32 m42 m13 m23 m33 m43 m14 m24 m34 m44


{-| Convert a record to a matrix.
-}
fromRecord : { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float } -> Mat4
fromRecord { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    Mat4 m11 m21 m31 m41 m12 m22 m32 m42 m13 m23 m33 m43 m14 m24 m34 m44
