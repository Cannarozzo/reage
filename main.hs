import Control.Monad.Logger (runStdoutLoggingT)
import Yesod
import Yesod.Form(Textarea)
import Database.Persist.TH( mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Database.Persist.MySQL
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock


import PersistentFields


             
data Reage = Reage{connPool :: ConnectionPool}

instance Yesod Reage

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario json
    apelido     Text
    senha       Text Maybe
    nome        Text
    anoNasci    Int
    email       Text
    estado      Text
    pais        Text
    avatar      Int
    cor         Int
    UniqueUsuario email
Publicacao json
    conteudo        Text
    dataCompleta    UTCTime 
    categoria       Bool
Categoria json
    categoria   CategoriaTipo
    descricao   Text
|]

mkYesod "Reage" [parseRoutes|
/ HomeR GET
/usuario/login                   UsuarioLoginR   POST OPTIONS
/usuario/obter/#UsuarioId        UsuarioGetR     GET
/usuario/inserir UsuarioInserirR POST       OPTIONS -- CADASTRO DO USUARIO, ALTERAÇÃO E DELETE,
/usuario/editar/#UsuarioId UsuarioConfR     PUT -- ALTERAÇÃO E DELETE
/usuario/remover/#UsuarioId UsuarioRemoverR DELETE

/publicar PublicarR  OPTIONS POST -- FAZER A PUBLICAÇÃO 
/feed FeedR GET -- MOSTRAR PUBLICAÇÃO NO FEED POR DATA
-- perfil PerfilR GET -- MOSTRAR PERFIL DO USUARIO E PUBLICAÇÕES FEITAS POR ELE
/buscar/#Text BuscaR GET



|]

instance YesodPersist Reage where
   type YesodPersistBackend Reage = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool
------------------------------------------------------
getHomeR :: Handler TypedContent 
getHomeR = selectRep $ do
               provideRep $ defaultLayout $ do
                   setTitle "Inicial"
                   [whamlet| 
                        <h1> Ola 
                    |]
               provideJson $ object ["saudacao" .= ("ola json" :: Text)]
--http://www.yesodweb.com/book/restful-contentstac

-- == usuario/login UsuarioLoginR
optionsUsuarioLoginR :: Handler ()
optionsUsuarioLoginR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "POST"

postUsuarioLoginR :: Handler Value
postUsuarioLoginR = do
    addHeader "Access-Control-Allow-Origin" "*"
    usuario <- requireJsonBody :: Handler Usuario
    usuarioTalvez <- runDB $ selectFirst [UsuarioEmail ==. (usuarioEmail usuario), UsuarioSenha ==. (usuarioSenha usuario)] []
    case usuarioTalvez of 
        Nothing -> 
            returnJson $ object ["resp" .= object ["usuario" .= pack "-1"] ]
        Just (Entity uid usuario) ->
            returnJson $ object ["resp" .= object ["usuario" .= uidText ] ]
            where 
                uidText = (pack.show.fromSqlKey) uid 
              
-- == usuario/obter UsuarioGetR 
getUsuarioGetR :: UsuarioId -> Handler ()
getUsuarioGetR uid = do
    usuario <- runDB $ get404 uid
    sendResponse $ toJSON usuario
  
  
-- == usuario/inserir UsuarioInserirR     
optionsUsuarioInserirR :: Handler ()
optionsUsuarioInserirR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "POST"

postUsuarioInserirR :: Handler Value
postUsuarioInserirR = do
    addHeader "Access-Control-Allow-Origin" "*"
    usuario <- requireJsonBody :: Handler Usuario
    usuarioid <- runDB $ insert usuario
    returnJson $ object [pack "resp" .= object [pack "CREATED" .= (pack $ show $ fromSqlKey usuarioid)] ] -- 

--  returnJson $ (object [pack "resp" .= pack "CREATED", pack "usuarioId" .= (pack $ show $ fromSqlKey usuarioid)])

-- == usuario/editar UsuarioConfR 
putUsuarioConfR :: UsuarioId -> Handler ()
putUsuarioConfR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    usuario <- requireJsonBody :: Handler Usuario
    runDB $ update cid [UsuarioApelido =. (usuarioApelido usuario),
                        UsuarioAnoNasci =. (usuarioAnoNasci usuario),
                        UsuarioEstado =. (usuarioEstado usuario),
                        UsuarioAvatar =. (usuarioAvatar usuario),
                        UsuarioCor =. (usuarioCor usuario)]
    sendResponse (object [pack "resp" .= pack "Changed"])
    

-- == usuario/
deleteUsuarioRemoverR :: UsuarioId -> Handler ()
deleteUsuarioRemoverR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ delete cid
    sendResponse (object [pack "resp".= pack "Deleted"])

optionsPublicarR :: Handler ()
optionsPublicarR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "POST"
    
postPublicarR :: Handler ()
postPublicarR = do
    addHeader "Access-Control-Allow-Origin" "*"
    publicacao <- requireJsonBody :: Handler Publicacao
    runDB $ insert publicacao
    sendResponse (object [pack "resp" .= pack "CREATED"])

    
getFeedR :: Handler ()
getFeedR = do
    addHeader "Access-Control-Allow-Origin" "*"
    publicacao <- runDB $ selectList [] [Asc PublicacaoDataCompleta]
    sendResponse (object["usuario" .= fmap toJSON publicacao])

    
    
getBuscaR :: Text -> Handler TypedContent
getBuscaR str = undefined
--    var <- runDB $ selectList [CategoriaTipo ==.str] [Asc PublicacaoDataReg]
--      sendResponse (object["usuario" .= fmap toJSON publicacao])

-----------------------------------------------------
--connStr = "user=babibonna password= host=localhost port=3306 dbname=reagedb "

connectionInfo = defaultConnectInfo { connectHost = "localhost",
                                      connectPort = 3306,
                                      connectUser = "babibonna",
                                      connectPassword = "",
                                      connectDatabase = "reagedb1"}
-- connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"
-- https://hackage.haskell.org/package/persistent-mysql-2.1/docs/Database-Persist-MySQL.html#t:ConnectInfo
main::IO()
main = runStdoutLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Reage pool)
       
       