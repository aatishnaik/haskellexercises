{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Test where 
import Lucid
import Data.Monoid
import Lucid.Base


mainPage :: Html()
mainPage = div_ (p_ "hello")

test1 :: Html ()
test1 = do
    "123 < 456 " :: Html () 
    "\n \n"
    script_ "alert('Hello!' > 12)" :: Html ()
    "\n \n"
    table_ (tr_ (td_ (p_ "Hello, World!"))) :: Html ()
    "\n \n"
    p_ "hello" <> p_ "sup" :: Html ()
    "\n \n"
    div_ (do p_ "hello"; p_ "sup") :: Html ()
    "\n \n"
    p_ [class_ "brand"] "Lucid Inc" :: Html ()
    "\n \n"
    p_ [data_ "zot" "foo",checked_] "Go!" :: Html ()
    "\n \n"
    style_ [style_ "inception"] "Go deeper." :: Html ()
    "\n \n"

page :: Html ()
page =
  html_
    (do head_
          (do title_ "Introduction page."
              link_ [rel_ "stylesheet",type_ "text/css",href_ "screen.css"]
              style_ "body{background:red}")
        body_
          (do div_ [id_ "header",style_ "color:white"] "Syntax"
              p_ (span_ (strong_ "This is an example of Lucid syntax."))
              hr_ []
              ul_ (mapM_ (li_ . toHtml . show)
                         [1,2,3])
              table_ (tr_ (do td_ "Hello!"
                              td_ [class_ "alt"] "World!"
                              td_ "Sup?"))))

people :: Html ()
people = ul_ (mapM_ person ["Mary Smith","Dave Jones"])
  where person name = li_ name