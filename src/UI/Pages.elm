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
    | Tasks
    | RankingRules
    | Synonyms Synonyms.Model
    | StopWords StopWords.Model
    | Attributes Attributes.Model


type alias Model =
    { documents : Page
    , settings : Page
    , synonyms : Page
    , stopWords : Page
    , attributes : Page
    }


init : String -> Model
init indexUid =
    { documents = Documents Documents.init
    , settings = Settings Settings.init
    , synonyms = Synonyms (Synonyms.init indexUid)
    , stopWords = StopWords StopWords.init
    , attributes = Attributes Attributes.init
    }


getPageList : Model -> List Page
getPageList model =
    [ model.documents, model.settings, model.synonyms, model.stopWords, model.attributes ]
