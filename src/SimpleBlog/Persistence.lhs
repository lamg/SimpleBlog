> {-# LANGUAGE UnicodeSyntax #-}

This module is the persistence interface of the Blog.
It permits storing and restoring the state of the Blog.

> module SimpleBlog.Persistence (store, restore) where
> import SimpleBlog.Blog (Blog)
> 
> store ∷ Blog → IO ()
> store = writeFile defaultFile . show
> 
> restore ∷ IO Blog
> restore = readFile defaultFile >>= return . read
> 
> defaultFile ∷ FilePath
> defaultFile = "Blog.state"
