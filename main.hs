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

share [mkPersist sqlSettings,  mkDeleteCascade sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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
    deriving Show
Publicacao json
    usuarioId       UsuarioId
    conteudo        Text
    dataCompleta    Text 
    categoria       CategoriaId
    deriving Show
Categoria json
    nome   Text -- CategoriaTipo, implementar o fromJson
    descricao   Text
    deriving Show
|]

mkYesod "Reage" [parseRoutes|
/ HomeR GET
/usuario/login                   UsuarioLoginR      POST OPTIONS
/usuario/obter/#UsuarioId        UsuarioGetR        GET OPTIONS
/usuario/inserir                 UsuarioInserirR    POST OPTIONS -- CADASTRO DO USUARIO, ALTERAÇÃO E DELETE,
/usuario/editar/#UsuarioId       UsuarioConfR       PUT -- ALTERAÇÃO E DELETE
/usuario/remover/#UsuarioId      UsuarioRemoverR    DELETE OPTIONS
/usuario/publicar                UsuarioPublicarR   POST OPTIONS  -- FAZER A PUBLICAÇÃO 

/publicacoes                     PublicacoesR       GET OPTIONS -- MOSTRAR PUBLICAÇÃO NO FEED POR DATA
-- /perfil                          PerfilR            GET OPTIONS -- MOSTRAR PERFIL DO USUARIO E PUBLICAÇÕES FEITAS POR ELE
/buscar/#Text                    BuscarR            GET OPTIONS



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
optionsUsuarioGetR :: UsuarioId -> Handler ()
optionsUsuarioGetR uid = do 
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "POST"

getUsuarioGetR :: UsuarioId -> Handler ()
getUsuarioGetR uid = do
    addHeader "Access-Control-Allow-Origin" "*" 
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
                        UsuarioEstado =. (usuarioEstado usuario)]
    sendResponse (object [pack "resp" .= pack "UPDATED"])
    

-- == usuario/remover
optionsUsuarioRemoverR :: UsuarioId -> Handler ()
optionsUsuarioRemoverR uid = do 
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "DELETE"

deleteUsuarioRemoverR :: UsuarioId -> Handler ()
deleteUsuarioRemoverR cid = do
    addHeader "Access-Control-Allow-Origin" "*"
    runDB $ get404 cid
    runDB $ deleteCascade cid
    sendResponse (object [pack "resp".= pack "Deleted"])

-- == usuario/postar UsuarioPostarR
optionsUsuarioPublicarR :: Handler ()
optionsUsuarioPublicarR = do 
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "POST"
    
postUsuarioPublicarR :: Handler ()
postUsuarioPublicarR = do
    addHeader "Access-Control-Allow-Origin" "*"
    agora <- liftIO $ getCurrentTime
    p <- requireJsonBody :: Handler Publicacao
    runDB $ insert ( Publicacao (publicacaoUsuarioId p) (publicacaoConteudo p) (pack "00-00-00") (publicacaoCategoria p) ) -- Implementar UTCTime
    sendResponse (object [pack "resp" .= pack "Postagem Criada"]) --felipe?
    
    
-- == publicacoes   PublicacoesR   
optionsPublicacoesR :: Handler ()
optionsPublicacoesR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "GET"
    
getPublicacoesR :: Handler Value
getPublicacoesR = do
    addHeader "Access-Control-Allow-Origin" "*"
    ps <- runDB $ selectList [] []
    us <- sequence $ fmap (runDB . get404 . publicacaoUsuarioId . entityVal) ps
    ct <- sequence $ fmap (runDB . get404 . publicacaoCategoria . entityVal) ps
    {-xs <- runDB $ (rawSql (pack $ "SELECT ??, ?? FROM usuario  \ 
        \ INNER JOIN publicacao \
        \ ON  usuario.id = publicacao.usuario_id ") []) :: Handler [(Entity Usuario,Entity Publicacao)]
    publicacao <- runDB $ selectList [] [Asc PublicacaoDataCompleta] -- Implementar por data de postagem (DESC)-}
    returnJson (object ["resp" .= usuarioPostagem (zip3 us ps ct)])
    where
        usuarioPostagem = fmap (\(u, p, c) -> object ["usuario" .= (toJSON u), "publicacao" .= (toJSON p), "categoria" .= (categoriaNome c)])
        zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
        zip3 (a:as) (b:bs) (c:cs) = [(a, b, c)] ++ zip3 as bs cs
        zip3 [] _ _ = []
            
    
-- == buscar
optionsBuscarR :: Text -> Handler ()
optionsBuscarR = undefined{-do
    addHeader "Access-Control-Allow-Origin" "*" 
    addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    addHeader "Access-Control-Allow-Methods" "GET"-}
    
getBuscarR :: Text -> Handler ()
getBuscarR str = do
    addHeader "Access-Control-Allow-Origin" "*"
    Just cat <- runDB $ selectFirst [CategoriaNome ==. str] []
    ps <- runDB $ selectList [PublicacaoCategoria ==. (entityKey cat)] []
    us <- sequence $ fmap (runDB . get404 . publicacaoUsuarioId . entityVal) ps
    sendResponse (object ["resp" .= categoriaPostagem (Prelude.zip us ps)])
    where
        categoriaPostagem = fmap (\(u, p) -> object ["usuario" .= (toJSON u), "publicacao" .= (toJSON p)])
    
-----------------------------------------------------
--connStr = "user=babibonna password= host=localhost port=3306 dbname=reagedb "

connectionInfo = defaultConnectInfo { connectHost = "localhost",
                                      connectPort = 3306,
                                      connectUser = "babibonna",
                                      connectPassword = "",
                                      connectDatabase = "reagedb"}
-- connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"
-- https://hackage.haskell.org/package/persistent-mysql-2.1/docs/Database-Persist-MySQL.html#t:ConnectInfo
main::IO()
main = runStdoutLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Reage pool)
       
       