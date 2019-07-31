import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
-- MAIN

main =
  Browser.element
    {
      init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Todo =
  {
    value : String,
    completed : Bool,
    index : Maybe Int
  }

type alias Model =
  {
    todos : List Todo
  , currentTodo : Todo
  , currentIndex : Int
  }

init : () -> ( Model, Cmd Msg)
init _ =
  ( {
    todos = []
  , currentIndex = 0
  , currentTodo = { value = "", completed = False,  index = Nothing }
  } , Cmd.none )



-- UPDATE

type Msg
  = AddTodo
  | UpdateCurrent String
  | Reset
  | ToggleChecked Todo

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AddTodo ->
      ( { model |
        todos = ((Todo model.currentTodo.value False (Just model.currentIndex)) :: model.todos)
        , currentTodo = { value = "", completed = False, index = Nothing }
        , currentIndex = model.currentIndex + 1 }
      , Cmd.none
      )
    UpdateCurrent newCurrentValue ->
      let
          newCurrent = { value = newCurrentValue, completed = False, index = Nothing}
      in
        ( { model | currentTodo = newCurrent }
        , Cmd.none
        )
    Reset ->
      ( { model | todos = [] }
      , Cmd.none
      )

    ToggleChecked updatedTodo ->
      let
          transform index todo =
            if todo.index == index then
              { todo | completed = (not todo.completed ) }
            else
              todo
      in
        ({ model | todos = ( List.map (transform updatedTodo.index) model.todos ) }, Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [ ] [ text "Todos" ]
    , addNewTodoSection model
    , renderTodos model.todos
    ]

addNewTodoSection : Model -> Html Msg
addNewTodoSection model =
  div []
    [
      input [ placeholder "Add todo", value model.currentTodo.value, onInput UpdateCurrent ] []
    , button [ onClick AddTodo ] [ text "Save" ]
    , button [ onClick Reset ] [ text "Reset List" ]
    ]


renderTodo : Todo -> Html Msg
renderTodo todo =
  div []
    [ text todo.value
    , input [ type_ "checkbox", onClick ( ToggleChecked todo ) ] []
    ]

renderTodos : List Todo -> Html Msg
renderTodos todos =
  div [] (List.map renderTodo todos)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none