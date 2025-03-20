module Main exposing (..)

import Browser
import Html exposing (Html, div, img, button, text)
import Html.Attributes exposing (src, class, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, int, list, string, bool, maybe, map3, map8)

-- Define a Product type
type alias Product =
    { id : Int
    , name : String
    , price : String
    , colors : List ColorOption
    , selectedColor : ColorOption
    , showInsideView : Bool
    , isValuePack : Bool  
    , description : String 
    }


-- Define a ColorOption type
type alias ColorOption =
    { colorCode : String
    , outsideImageUrl : String
    , insideImageUrl : String
    }

-- Model
type alias Model =
    { products : List Product
    , error : Maybe String
    , currentIndex : Int
    }

-- Initialize Model
init : () -> (Model, Cmd Msg)
init _ =
    ( { products = [], error = Nothing, currentIndex = 0 }
    , fetchProducts
    )

-- JSON Decoders
colorOptionDecoder : Decoder ColorOption
colorOptionDecoder =
    Json.Decode.map3 ColorOption
        (field "colorCode" string)
        (field "outsideImageUrl" string)
        (field "insideImageUrl" string)

productDecoder : Decoder Product
productDecoder =
    Json.Decode.map8 Product
        (field "id" int)
        (field "name" string)
        (field "price" string)
        (field "colors" (list colorOptionDecoder))
        (Json.Decode.succeed { colorCode = "#FFFFFF", outsideImageUrl = "", insideImageUrl = "" }) -- Default selected color
        (Json.Decode.succeed False) -- Default showInsideView
        (field "isValuePack" (maybe bool) |> Json.Decode.map (Maybe.withDefault False)) -- Default False
        (field "description" (maybe string) |> Json.Decode.map (Maybe.withDefault ""))

productsDecoder : Decoder (List Product)
productsDecoder =
    list productDecoder

-- Fetch Products from API
fetchProducts : Cmd Msg
fetchProducts =
    Http.get
        { url = "http://127.0.0.1:8000/products.json"
        , expect = Http.expectJson FetchProductsResponse productsDecoder
        }

-- Messages
type Msg
    = FetchProductsResponse (Result Http.Error (List Product))
    | SelectColor Int ColorOption
    | ToggleInsideView Int
    | PreviousProduct 
    | NextProduct     

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        --  Fetch Products & Auto-Select First Color
        FetchProductsResponse (Ok products) ->
            let
                updatedProducts =
                    List.map
                        (\p ->
                            case p.colors of
                                firstColor :: _ -> { p | selectedColor = firstColor, showInsideView = False }
                                [] -> p
                        )
                        products
            in
            ( { model | products = updatedProducts, error = Nothing, currentIndex = 0 }, Cmd.none )

        --  Handle Fetch Error
        FetchProductsResponse (Err _) ->
            ( { model | error = Just "Failed to fetch products" }, Cmd.none )

        --  Handle Color Selection
        SelectColor productId color ->
            let
                updatedProducts =
                    List.map
                        (\p ->
                            if p.id == productId then
                                { p | selectedColor = color, showInsideView = False }
                            else
                                p
                        )
                        model.products
            in
            ( { model | products = updatedProducts }, Cmd.none )

        --  Toggle Inside View
        ToggleInsideView productId ->
            let
                updatedProducts =
                    List.map
                        (\p ->
                            if p.id == productId then
                                { p | showInsideView = not p.showInsideView }
                            else
                                p
                        )
                        model.products
            in
            ( { model | products = updatedProducts }, Cmd.none )

        -- Move to Previous Product (Looping Back)
        PreviousProduct ->
            let
                prevIndex =
                    if model.currentIndex > 0 then
                        model.currentIndex - 1
                    else
                        List.length model.products - 1  -- Loop back to last product
            in
            ( { model | currentIndex = prevIndex }, Cmd.none )

        -- Move to Next Product (Looping Forward)
        NextProduct ->
            let
                nextIndex =
                    if model.currentIndex < List.length model.products - 1 then
                        model.currentIndex + 1
                    else
                        0  -- Loop back to first product
            in
            ( { model | currentIndex = nextIndex }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "carousel-container" ]
        [ -- Left Arrow (Placed outside products)
          button [ class "arrow left-arrow", onClick PreviousProduct ] [ text "<" ]

        -- Display Three Products at a Time
        , div [ class "carousel-track" ]
            (List.take 3 (List.drop model.currentIndex model.products)
                |> List.map viewProduct
            )

        -- Right Arrow (Placed outside products)
        , button [ class "arrow right-arrow", onClick NextProduct ] [ text ">" ]
        ]



viewProduct : Product -> Html Msg
viewProduct product =
    let
        selectedImage =
            if product.showInsideView then
                product.selectedColor.insideImageUrl
            else
                product.selectedColor.outsideImageUrl
    in
    div [ class "product-card" ]
        [ -- Product Image
          img [ src selectedImage, class "product-image" ] []

        -- "Value Pack" Button OR "Show Inside" Button (Appears on Hover)
        , if product.isValuePack then
            div [ class "value-pack-button" ] [ text "Value Pack" ]
          else
            div [ class "view-inside-button", onClick (ToggleInsideView product.id) ]
                [ text (if product.showInsideView then "Close X" else "+ Show Inside") ]

        -- Product Name & Price
        , div [ class "product-info" ]
            [ div [] [ text product.name ]
            , div [] [ text product.price ]
            ]

        -- Color Options (Highlight Selected)
        , div [ class "color-options" ]
            (List.map (\color ->
                button
                    [ class "color-circle"
                    , style "background-color" color.colorCode
                    , class (if color.colorCode == product.selectedColor.colorCode then "active-color" else "")
                    , onClick (SelectColor product.id color)
                    ]
                    []
            ) product.colors)

        -- Small Description (Below Color Options)
        , div [ class "product-description" ] [ text product.description ]
        ]


-- Main
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
