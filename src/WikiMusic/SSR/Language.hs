{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.Language
  ( DictTerm (..),
    Titles (..),
    Errors (..),
    Slogans (..),
    ApplicationDictionary (..),
    More (..),
    Forms (..),
    Sortings (..),
    towerOfBabel,
    dictionary,
    (|##|),
  )
where

import Data.Text (unpack)
import Optics
import Relude
import WikiMusic.SSR.Model.Api

data DictTerm = DictTerm
  { en :: Text,
    nl :: Text
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''DictTerm

data Titles = Titles
  { wikimusicSSR :: DictTerm,
    errorOccurred :: DictTerm,
    artistsPage :: DictTerm,
    genresPage :: DictTerm,
    songsPage :: DictTerm,
    login :: DictTerm
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Titles

data Errors = Errors
  { clientError :: DictTerm
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Errors

data More = More
  { likes :: DictTerm,
    dislikes :: DictTerm,
    artistsNav :: DictTerm,
    genresNav :: DictTerm,
    songsNav :: DictTerm,
    loginNav :: DictTerm,
    views :: DictTerm,
    warningHeavyDevelopment :: DictTerm,
    createdBy :: DictTerm,
    createdAt :: DictTerm,
    lastEditedAt :: DictTerm,
    musicTuning :: DictTerm,
    musicKey :: DictTerm,
    musicCreationDate :: DictTerm,
    albumName :: DictTerm,
    albumInfoLink :: DictTerm,
    irreversibleAction :: DictTerm,
    copyright0 :: DictTerm,
    copyright1 :: DictTerm,
    copyright2 :: DictTerm
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''More

data Slogans = Slogans
  { pageTop :: DictTerm
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Slogans

data Forms = Forms
  { email :: DictTerm,
    password :: DictTerm,
    submit :: DictTerm,
    delete :: DictTerm
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Forms

data Sortings = Sortings
  { alphabeticalAsc :: DictTerm,
    alphabeticalDesc :: DictTerm,
    createdAtAsc :: DictTerm,
    createdAtDesc :: DictTerm,
    lastEditedAtAsc :: DictTerm,
    lastEditedAtDesc :: DictTerm
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Sortings

data Buttons = Buttons
  { edit :: DictTerm,
    delete :: DictTerm,
    like :: DictTerm,
    dislike :: DictTerm
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Buttons

data ApplicationDictionary = ApplicationDictionary
  { titles :: Titles,
    errors :: Errors,
    slogans :: Slogans,
    more :: More,
    forms :: Forms,
    sortings :: Sortings,
    buttons :: Buttons
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''ApplicationDictionary

dictionary :: ApplicationDictionary
dictionary =
  ApplicationDictionary
    { titles =
        Titles
          { wikimusicSSR = DictTerm {en = "WikiMusic", nl = "WikiMusic"},
            errorOccurred = DictTerm {en = "Error occurred!", nl = "Fout opgetreden"},
            artistsPage = DictTerm {en = "Artists", nl = "Artiesten"},
            genresPage = DictTerm {en = "Genres", nl = "Genres"},
            songsPage = DictTerm {en = "Songs", nl = "Nummers"},
            login = DictTerm {en = "Login", nl = "Inloggen"}
          },
      forms =
        Forms
          { email = DictTerm {en = "email address:", nl = "emailadres:"},
            password = DictTerm {en = "password:", nl = "wachtwoord:"},
            submit = DictTerm {en = "submit", nl = "indienen"},
            delete = DictTerm {en = "delete", nl = "verwijderen"}
          },
      errors =
        Errors
          { clientError = DictTerm {en = "Client error!", nl = "Client error!"}
          },
      buttons =
        Buttons
          { edit = DictTerm {en = "edit", nl = "bijwerken"},
            delete = DictTerm {en = "delete", nl = "verwijderen"},
            like = DictTerm {en = "like", nl = "leuk"},
            dislike = DictTerm {en = "dislike", nl = "niet leuk"}
          },
      slogans =
        Slogans
          { pageTop =
              DictTerm
                { en = "Welcome to the music knowledge sharing community 🎼",
                  nl = "Welkom bij de gemeenschap waar je muziek kennis kan delen 🎼"
                }
          },
      sortings =
        Sortings
          { alphabeticalAsc = DictTerm {en = "alphabetical - asc", nl = "alfabetisch - opl"},
            alphabeticalDesc = DictTerm {en = "alphabetical - desc", nl = "alfabetisch - afl"},
            createdAtAsc = DictTerm {en = "created at - asc", nl = "datum gemaakt - opl"},
            createdAtDesc = DictTerm {en = "created at - desc", nl = "datum gemaakt - afl"},
            lastEditedAtAsc = DictTerm {en = "last edited - asc", nl = "laatst bijgewerkt - opl"},
            lastEditedAtDesc = DictTerm {en = "last edited - desc", nl = "laatst bijgewerkt - afl"}
          },
      more =
        More
          { likes =
              DictTerm
                { en = "likes",
                  nl = "leuks"
                },
            dislikes =
              DictTerm
                { en = "dislikes",
                  nl = "niet leuks"
                },
            artistsNav =
              DictTerm
                { en = "Artists",
                  nl = "Artiesten"
                },
            genresNav =
              DictTerm
                { en = "Genres",
                  nl = "Genres"
                },
            songsNav =
              DictTerm
                { en = "Songs",
                  nl = "Nummers"
                },
            loginNav =
              DictTerm
                { en = "Login",
                  nl = "Inloggen"
                },
            views =
              DictTerm
                { en = "views",
                  nl = "weergaven"
                },
            createdBy =
              DictTerm
                { en = "added by:",
                  nl = "toegevoegd door:"
                },
            createdAt =
              DictTerm
                { en = "date created:",
                  nl = "datum gemaakt:"
                },
            lastEditedAt =
              DictTerm
                { en = "last edited at:",
                  nl = "laatst bijgewerkt:"
                },
            warningHeavyDevelopment = DictTerm {en = "Warning! WikiMusic is ongoing an experimental stage! Certain features will not work and are being fixed.", nl = "Let op! WikiMusic is momenteel in een experimentele periode. Hou rekening met de feit dat sommige functies niet zullen werken, er wordt aan gewerkt. "},
            musicTuning = DictTerm {en = "tuning:", nl = "stemming:"},
            musicKey = DictTerm {en = "music key:", nl = "muziek key:"},
            musicCreationDate = DictTerm {en = "music creation:", nl = "muziek geschreven:"},
            albumName = DictTerm {en = "album name:", nl = "album naam:"},
            albumInfoLink = DictTerm {en = "album info:", nl = "info over album:"},
            irreversibleAction =
              DictTerm
                { en = "Are you sure you wish to proceed with this irreversible action?",
                  nl = "Wilt u zeker dat u verder wil gaan? Deze actie kan niet terug worden gedraaid!"
                },
            copyright0 =
              DictTerm
                { en = "Copyright - J.J. Bigorra (jjbigorra@gmail.com)",
                  nl = "Copyright - J.J. Bigorra (jjbigorra@gmail.com)"
                },
            copyright1 =
              DictTerm
                { en = "See the source code behind WikiMusic here",
                  nl = "Zie hier de broncode achter WikiMusic"
                },
            copyright2 =
              DictTerm
                { en = "WikiMusic is completely licensed under the GNU General Public License v3 or later",
                  nl = "WikiMusic is volledig onder de GNU General Public License v3 of nieuwer gelicensieerd"
                }
          }
    }

translateTerm :: Text -> DictTerm -> Text
translateTerm "nl" x = x ^. #nl
translateTerm _ x = x ^. #en

towerOfBabel :: Language -> DictTerm -> Text
towerOfBabel language x = fromString . unpack $ translateTerm (language ^. #value) x

-- towerOfBabel alias
infixl 8 |##|

(|##|) :: (ApplicationDictionary -> DictTerm) -> Language -> Text
(|##|) fromDict language = towerOfBabel language (fromDict dictionary)
