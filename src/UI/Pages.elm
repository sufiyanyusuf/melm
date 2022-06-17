module UI.Pages exposing (..)

import Element exposing (..)
import UI.PageViews.Attributes as Attributes
import UI.PageViews.Documents as Documents
import UI.PageViews.Settings as Settings
import UI.PageViews.StopWords as StopWords
import UI.PageViews.Synonyms as Synonyms


type Page
    = Settings Settings.Model
    | Documents Documents.Model
    | Synonyms Synonyms.Model
    | StopWords StopWords.Model
    | Attributes Attributes.Model


type alias Model =
    { documents : Documents.Model
    , settings : Settings.Model
    , synonyms : Synonyms.Model
    , stopWords : StopWords.Model
    , attributes : Attributes.Model
    , selectedPage : Page
    }


init : String -> Model
init indexUid =
    { documents = Documents.init
    , settings = Settings.init
    , synonyms = Synonyms.init
    , stopWords = StopWords.init
    , attributes = Attributes.init
    , selectedPage = Documents Documents.init
    }


getPageList : Model -> List Page
getPageList model =
    [ Documents model.documents
    , Synonyms model.synonyms
    , StopWords model.stopWords
    , Attributes model.attributes
    , Settings model.settings
    ]
