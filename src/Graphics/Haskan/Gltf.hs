{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

--{-# language DuplicateRecordFields #-}
--{-# language DeriveGeneric #-}
--{-# language FlexibleInstances #-}
module Graphics.Haskan.Gltf where

-- base
import GHC.Generics

-- aeson
import Data.Aeson (eitherDecodeFileStrict)
--import Data.Aeson
--import Data.Aeson.Types

-- aeson-schemas
import Data.Aeson.Schema

-- text
import Data.Text (Text)
import qualified Data.Text as T

-- linear
import Linear (M44(..), V3(..), V4(..))

-- vector
import qualified Data.Vector as V

{--
type Gltf = [schema|
  {
    extensionsUsed: Maybe List Text,
    extensionsRequired: Maybe List Text,
    accessors: Maybe List {
      bufferView: Maybe Int,
      byteOffset: Maybe Int, //default 0
      componentType: Int,
      normalized: Maybe Bool, //default False
      count: Int,
      type: Text,
      max: Maybe List Float,
      min: Maybe List Float,
      sparse: Maybe {
        count: Int,
        indices: {
          bufferView: Int,
          byteOffset: Maybe Int, //default 0
          componentType: Int,
          extensions: Maybe {
            key: Text, //TODO: define list of objects
          },
          extras: Maybe {
            key: Text, //TODO: define list of objects
          }
        },
        values: {
          bufferView: Int,
          byteOffset: Maybe Int, //default 0
          extensions: Maybe {
            key: Text, //TODO: define list of objects
          },
          extras: Maybe {
            key: Text, //TODO: define list of objects
          }
        },
        extensions: Maybe {
          key: Text, //TODO: define list of objects
        },
        extras: Maybe {
          key: Text, //TODO: define list of objects
        },
        name: Maybe Text
      },
    },
    asset: {
      version: Text,
      generator: Text
    }
  }|]

test :: IO ()
test = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "data/scene1.gltf" :: IO (Object Gltf)

  print [get| obj.asset.version |]
  print [get| obj.extensionsUsed |]
  print [get| obj.accessors |]
{--
type Version = Text

data Transformation
  = Transform (M44 Float)
  | TRS { translation :: Maybe (V3 Float)
        , rotation :: Maybe (V4 Float)
        , scale :: Maybe (V3 Float) }
  deriving Show

parseJSONElemAtIndex :: (Value -> Parser a) -> Int -> V.Vector Value -> Parser a
parseJSONElemAtIndex p idx ary = p (V.unsafeIndex ary idx) <?> Index idx

instance FromJSON (V4 Float) where
  parseJSON = withArray "V4 Float" $ \xs ->
    let
      n = V.length xs
    in
      if n == 4
      then
        V4
        <$> parseJSONElemAtIndex parseJSON 0 xs
        <*> parseJSONElemAtIndex parseJSON 1 xs
        <*> parseJSONElemAtIndex parseJSON 2 xs
        <*> parseJSONElemAtIndex parseJSON 3 xs
      else
        fail $ "cannot unpack array of length " ++ show n ++ " into a V4"

instance FromJSON (V3 Float) where
  parseJSON = withArray "V3 Float" $ \xs ->
    let
      n = V.length xs
    in
      if n == 3
      then
        V3
        <$> parseJSONElemAtIndex parseJSON 0 xs
        <*> parseJSONElemAtIndex parseJSON 1 xs
        <*> parseJSONElemAtIndex parseJSON 2 xs
      else
        fail $ "cannot unpack array of length " ++ show n ++ " into a V3"

instance FromJSON (M44 Float) where
  parseJSON = withArray "M44 Float" $ \xs ->
    let
      n = V.length xs
    in
      if n == 16
      then do
        v1 <- V4
              <$> parseJSONElemAtIndex parseJSON 0 xs
              <*> parseJSONElemAtIndex parseJSON 1 xs
              <*> parseJSONElemAtIndex parseJSON 2 xs
              <*> parseJSONElemAtIndex parseJSON 3 xs
        v2 <- V4
              <$> parseJSONElemAtIndex parseJSON 4 xs
              <*> parseJSONElemAtIndex parseJSON 5 xs
              <*> parseJSONElemAtIndex parseJSON 6 xs
              <*> parseJSONElemAtIndex parseJSON 7 xs
        v3 <- V4
              <$> parseJSONElemAtIndex parseJSON 8 xs
              <*> parseJSONElemAtIndex parseJSON 9 xs
              <*> parseJSONElemAtIndex parseJSON 10 xs
              <*> parseJSONElemAtIndex parseJSON 11 xs
        v4 <- V4
              <$> parseJSONElemAtIndex parseJSON 12 xs
              <*> parseJSONElemAtIndex parseJSON 13 xs
              <*> parseJSONElemAtIndex parseJSON 14 xs
              <*> parseJSONElemAtIndex parseJSON 15 xs
        pure (V4 v1 v2 v3 v4)

      else
        fail $ "cannot unpack array of length " ++ show n ++ " into a M44"

data Asset = Asset { version :: Version, minVersion :: Maybe Version, generator :: Maybe Text, copyright :: Maybe Text }
  deriving (Show, Generic)

instance FromJSON Asset
instance ToJSON Asset where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

data Scene
  = Scene { name :: Maybe Text
          , nodes :: [Int]
          }
  deriving (Show, Generic)

instance FromJSON Scene

data Node
  = Node { name :: Maybe Text
         , transformation :: Transformation
         , camera :: Maybe Int
         }
--}
{--
         , translation :: Maybe (Float, Float, Float)
         , rotation :: Maybe (Float, Float, Float, Float)
         , scale :: Maybe (Float, Float, Float)
         , matrix :: Maybe (Float, Float, Float, Float
                           ,Float, Float, Float, Float
                           ,Float, Float, Float, Float
                           ,Float, Float, Float, Float)
--}
  deriving (Show, Generic)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> do
    name <- v .:? "name"
    camera <- v .:? "camera"
    matrix <- v .:? "matrix"
    t <- v .:? "translation"
    r <- v .:? "rotation"
    s <- v .:? "scale"
    let trans = case matrix of
                  Nothing -> TRS t r s
                  Just m -> Transform m
    pure (Node name trans camera)

{-
instance ToJSON Node where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
-}
data Gltf
  = Gltf { asset :: Asset
         , scene :: Maybe Int
         , scenes :: [Scene]
         , nodes :: [Node]
         }
  deriving (Show, Generic)

instance FromJSON Gltf
--}
