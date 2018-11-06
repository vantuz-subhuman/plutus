module MainFrame
  ( mainFrame
  ) where

import Ace.EditSession as Session
import Ace.Editor as Editor
import Ace.Halogen.Component (AceEffects, AceMessage(..), AceQuery(..), Autocomplete(..), aceComponent)
import Ace.Types (ACE, Editor, Annotation)
import Action (actionsPane)
import Bootstrap (btn, btnDanger, btnPrimary, btnSecondary, btnSuccess, col2_, col7_, col_, container_, listGroupItem_, listGroup_, row_)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Lens (assign, modifying, use)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show (show)
import Data.String as String
import Data.Tuple.Nested ((/\))
import ECharts.Commands as E
import ECharts.Monad (CommandsT, interpret)
import ECharts.Types.Phantom (I)
import Halogen (Component)
import Halogen as H
import Halogen.Component (ParentHTML)
import Halogen.Component.ChildPath (ChildPath, cp1, cp2)
import Halogen.ECharts (EChartsEffects, EChartsQuery, echarts)
import Halogen.ECharts as EC
import Halogen.HTML (ClassName(ClassName), HTML, a, br_, button, div, div_, h1_, h2_, h3_, hr_, p_, slot', text)
import Halogen.HTML.Events (input, input_, onClick)
import Halogen.HTML.Properties (class_, classes, disabled, href, target)
import Halogen.Query (HalogenM)
import Icons (Icon(..), icon)
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..), isLoading)
import Network.RemoteData as RemoteData
import Playground.API (CompilationError(..), SourceCode(..))
import Playground.Server (SPParams_, postContract)
import Prelude (class Eq, class Monad, class Ord, type (~>), Unit, Void, bind, const, discard, flip, pure, unit, void, ($), (+), (<$>), (<*>), (<<<))
import Servant.PureScript.Settings (SPSettings_)
import StaticData as Static
import Types (Query(..), State, WalletId(..), _actions, _compilationResult, _editorContents, _wallets)
import Wallet (walletsPane)

initialState :: State
initialState =
  { actions: []
  , wallets: Static.wallets
  , editorContents: Static.editorContents
  , compilationResult: NotAsked
  }

type ChildQuery = Coproduct2 AceQuery EChartsQuery
type ChildSlot = Either2 AceSlot EChartsSlot

data AceSlot = AceSlot
derive instance eqComponentAceSlot :: Eq AceSlot
derive instance ordComponentAceSlot :: Ord AceSlot

data EChartsSlot = EChartsSlot
derive instance eqComponentEChartsSlot :: Eq EChartsSlot
derive instance ordComponentEChartsSlot :: Ord EChartsSlot

cpAce :: ChildPath AceQuery ChildQuery AceSlot ChildSlot
cpAce = cp1

cpECharts :: ChildPath EChartsQuery ChildQuery EChartsSlot ChildSlot
cpECharts = cp2

------------------------------------------------------------

mainFrame ::
  forall m aff.
  MonadAff (EChartsEffects (AceEffects (ajax :: AJAX | aff))) m
  => MonadAsk (SPSettings_ SPParams_) m
  => Component HTML Query Unit Void m
mainFrame =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

eval ::
  forall m aff.
  MonadAff (ace :: ACE, ajax :: AJAX | aff) m
  => MonadAsk (SPSettings_ SPParams_) m
  => Query ~> HalogenM State Query ChildQuery ChildSlot Void m
eval (HandleAceMessage (TextChanged text) next) = do
  assign _editorContents text
  pure next

eval (HandleEChartsMessage EC.Initialized next) = do
  void $ H.query' cpECharts EChartsSlot $ H.action $ EC.Set $ interpret sankeyDiagramOptions
  pure next

-- We just ignore most ECharts events.
eval (HandleEChartsMessage (EC.EventRaised event) next) =
  pure next

eval (CompileProgram next) = do
  contents <- use _editorContents
  --
  assign _compilationResult Loading
  result <- runAjax $ postContract $ SourceCode contents
  assign _compilationResult result
  --
  withEditor $ showCompilationErrorAnnotations $
    case result of
      Success (Left errors) -> errors
      _ -> []
  --
  pure next

eval (ScrollToRow row next) = do
  withEditor $ Editor.scrollToLine row true true (pure unit)
  pure next

eval (SendAction action next) = do
  modifying _actions $ flip Array.snoc action
  pure next

eval (KillAction index next) = do
  modifying _actions (fromMaybe <*> Array.deleteAt index)
  pure next

eval (AddWallet next) = do
  count <- Array.length <$> use _wallets
  let newWallet =
        { walletId: WalletId (show (count + 1))
        , balance: 10.0
        }
  modifying _wallets (flip Array.snoc newWallet)
  pure next

eval (RemoveWallet index next) = do
  modifying _wallets (fromMaybe <*> Array.deleteAt index)
  assign _actions []
  pure next

------------------------------------------------------------

-- | Handles the messy business of running an editor command iff the
-- editor is up and running.
withEditor :: forall m eff.
  MonadEff (ace :: ACE | eff) m
  => (Editor -> Eff (ace :: ACE | eff) Unit)
  -> HalogenM State Query ChildQuery ChildSlot Void m Unit
withEditor action = do
  mEditor <- H.query' cpAce AceSlot $ H.request GetEditor
  case mEditor of
    Just (Just editor) -> liftEff $ action editor
    _ -> pure unit

showCompilationErrorAnnotations :: forall m.
  Array CompilationError
  -> Editor
  -> Eff (ace :: ACE | m) Unit
showCompilationErrorAnnotations errors editor = do
  session <- Editor.getSession editor
  Session.setAnnotations (catMaybes (toAnnotation <$> errors)) session

toAnnotation :: CompilationError -> Maybe Annotation
toAnnotation (RawError _) = Nothing
toAnnotation (CompilationError {row, column, text}) =
  Just
    { type: "error"
    , row
    , column
    , text: String.joinWith "\n" text
    }

initEditor ∷
  forall m aff.
  MonadAff (ace :: ACE | aff) m
  => String -> Editor -> m Unit
initEditor contents editor = liftEff $ do
  void $ Editor.setValue contents (Just 1) editor
  --
  session <- Editor.getSession editor
  Session.setMode "ace/mode/haskell" session

render ::
  forall m aff.
  MonadAff (EChartsEffects (AceEffects aff)) m
  => State -> ParentHTML Query ChildQuery ChildSlot m
render state =
  div [ class_ (ClassName "main-frame") ] $
    [ container_
      [ header
      , hr_
      , editorPane state
      , hr_
      , mockChainPane state
      ]
    ]

header :: forall p i. HTML p i
header =
  row_
    [ col_ [ h1_ [ text "Plutus Playground" ] ]
    , col_
      [ p_ [
          a [ href "https://github.com/input-output-hk/plutus/tree/mchakravarty/plutus-playground-spec/docs/playground"
            , target "_blank"
            ]
            [ text "Design Document" ]
          ]
      ]
    ]

editorPane ::
  forall m aff.
  MonadAff (EChartsEffects (AceEffects aff)) m
  => State -> ParentHTML Query ChildQuery ChildSlot m
editorPane state =
  div_
    [ slot' cpAce AceSlot
        (aceComponent (initEditor state.editorContents) (Just Live))
        unit
        (input HandleAceMessage)
    , br_
    , row_
        [ col2_
            [ button [ classes [ btn
                               , case state.compilationResult of
                                   Success (Right _) -> btnSuccess
                                   Success (Left _) -> btnDanger
                                   Loading -> btnSecondary
                                   _ -> btnPrimary
                               ]
                     , onClick $ input_ CompileProgram
                     , disabled (isLoading state.compilationResult)
                     ]
                [ if (isLoading state.compilationResult)
                    then icon Spinner
                    else text "Compile"
                ]
            ]
       , col_
           [ case state.compilationResult of
               (Success (Left errors)) ->
                 listGroup_
                   (listGroupItem_ <<< pure <<< compilationErrorPane <$> errors)
               _ -> text ""
           ]
       ]
    ]

compilationErrorPane :: forall p. CompilationError -> HTML p (Query Unit)
compilationErrorPane (RawError error) =
  div_ [ text error ]
compilationErrorPane (CompilationError error) =
  div [ onClick $ input_ $ ScrollToRow error.row ]
    [ h2_ [ text error.filename ]
    , div_ [ text $ String.joinWith "\n" error.text ]
    ]

mockChainPane ::
  forall m aff.
  MonadAff (EChartsEffects (AceEffects aff)) m
  => State -> ParentHTML Query ChildQuery ChildSlot m
mockChainPane state =
  div_
    [ row_
        [ col7_ [ walletsPane state.wallets ]
        , col_ [ actionsPane state.actions  ]
        ]
    , h3_ [ text "Chain" ]
    , slot' cpECharts EChartsSlot
        (echarts Nothing)
        ({width: 800, height: 800} /\ unit)
        (input HandleEChartsMessage)
    ]

sankeyDiagramOptions :: forall m i. Monad m => CommandsT (series :: I | i) m Unit
sankeyDiagramOptions = do
  E.series $ E.sankey do
    E.buildItems do
      E.addItem do
        E.name "charles"
        E.value 600.0
      E.addItem do
        E.name "kris"
        E.value 10.0
      E.addItem do
        E.name "david"
        E.value 15.0
      E.addItem do
        E.name "manuel"
        E.value 123.0
    E.buildLinks do
      E.addLink do
        E.sourceName "charles"
        E.targetName "kris"
        E.value 10.0
      E.addLink do
        E.sourceName "charles"
        E.targetName "david"
        E.value 10.0
      E.addLink do
        E.sourceName "charles"
        E.targetName "manuel"
        E.value 20.0

      E.addLink do
        E.sourceName "manuel"
        E.targetName "kris"
        E.value 5.0
      E.addLink do
        E.sourceName "manuel"
        E.targetName "david"
        E.value 5.0

runAjax ::
  forall m env a e.
  MonadAsk env m
  => ReaderT env (ExceptT e m) a -> m (RemoteData e a)
runAjax action = do
  settings <- ask
  result <- runExceptT $ runReaderT action settings
  pure $ RemoteData.fromEither result
