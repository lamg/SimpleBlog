> {-# LANGUAGE UnicodeSyntax #-}
> module SimpleBlog.Blog
>        (
>          Blog,
>          Comment,
>          Post,
>          visit,
>          read,
>          post,
>          comment
>        )
> where
> import Prelude hiding (read)
> import Data.Maybe (listToMaybe)

Main Blog interface
----
A blog can be visited, meaning obtaining a summary
of all posts in a way amenable to being displayed.

> visit ∷ Blog → [Summary]
> visit = map summary . posts

Given a post's identification its contents can
be read.

> read ∷ Blog → Title → Maybe Post
> read = flip getPost

Given a post's identification it can be commented.

> comment ∷ Comment → Title → Blog → Maybe Blog
> comment c i b = getPost i b >>= return . addComment c >>= return . modifyPost b

> getPost ∷ Title → Blog → Maybe Post
> getPost i = listToMaybe . filter ((== i) . title) . posts
> 
> title ∷ Post → Title
> title (Post t _ _ _) = t
 
So far I think that is enough for a simple blog. Now I define
the previously undefined data and functions.

Definitions triggered by visit function
----
The Blog data type is meant to store all the information. The
visit function obliges to define posts ∷ Blog → [Post], that
can be done defining Blog as a record.

> data Blog = Blog { posts ∷ [Post] } deriving (Show, Read)
> 
> data Post = Post Title Author Content [Comment] deriving (Show, Read)
> type Title = String
> type Author = String
> type Content = String
>
> data Comment = Comment Author Content deriving (Show, Read)
> 
> summary ∷ Post → Summary
> summary (Post t a c _) = Summary t a . unwords . take 20 . words $ c
> 
> data Summary = Summary Title Author Content deriving (Show)

Definitions triggered by comment function
----

> addComment ∷ Comment → Post → Post
> addComment c (Post x y z cs) = Post x y z $ cs ++ [c]
> 
> post ∷ Blog → Post → Blog
> post (Blog ps) p = Blog $ ps ++ [p]
> 
> modifyPost ∷ Blog → Post → Blog
> modifyPost (Blog ps) p = Blog $
>                          replace (\x → title p == title x) p ps
> 
> replace ∷ (a → Bool) → a → [a] → [a]
> replace _ _ [] = []
> replace p e (x:xs) | p x = e:replace p e xs
>                    | otherwise = x:replace p e xs
