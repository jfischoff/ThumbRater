fmt :: EditScriptL AppStateTupleFam txs tys -> Doc e
fmt (Cpy c d)   = fmt_ast_cpy c d
--fmt (CpyTree d) = text " ... "  <+> fmt d
--fmt (Del c d)   = text (color Red   ("-" ++ string c)) <+> text " " <+> fmt d
--fmt (Ins c d)   = text (color Green ("+" ++ string c)) <+> text " " <+> fmt d
--fmt (End)       = empty

fmt_ast_cpy c@(AppStateTuple') d = text (string c) <+> (indent 4 $ fmt_context_list_start d)

fmt_context_list_start (Cpy c d)   = fmt_context_list_start_cpy c d
fmt_context_list_start _ = error "tree context not cpy"


fmt_context_list_start_cpy c@(ContextList')   d = text (string c) <+> text "[" <+> (indent 4 $ fmt_context_list_element d)
fmt_context_list_start_cpy _ _ = error "empty context list!"

fmt_context_list_element :: EditScriptL AppStateTupleFam t t1 -> Doc e
fmt_context_list_element (Cpy GraphContext' d)    = text "," </> fmt_context_adj black d 
fmt_context_list_element (Cpy NilContextList' d) = text "]" <+> line <+> fmt_leaderboards d
fmt_context_list_element (CpyTree d)             = fmt_leaderboards d
fmt_context_list_element (Del GraphContext' d)    = start_color Red <+> text "," </> fmt_adj_list_start d
fmt_context_list_element (Del NilContextList' d) = color Red (text "]") <+> line <+> fmt_leaderboards d
fmt_context_list_element (Ins GraphContext' d)    = start_color Green <+> text "," </> fmt_adj_list_start d
fmt_context_list_element (Ins NilContextList' d) = color Green (text "]") <+> line <+> fmt_leaderboards d
fmt_context_list_element (Cpy c _) = error ("what is " ++ string c)

fmt_adj_list_start (Cpy AdjList' d) = text "[" <+> fmt_context_adj black d
fmt_adj_list_start (Cpy NilAdjList' d) = text "]" <+> fmt_context_node d
fmt_adj_list_start (Del AdjList' d) = text "[" <+> fmt_context_adj black d
fmt_adj_list_start (Del NilAdjList' d) = text "]" <+> fmt_context_node d
fmt_adj_list_start (Ins AdjList' d) = text "[" <+> fmt_context_adj black d
fmt_adj_list_start (Ins NilAdjList' d) = text "]" <+> fmt_context_node d


--fmt_context end_color (Cpy GraphContext' d) = text "(" <+> fmt_adj_list d <+> text ")" <+> end_color
fmt_context_adj end_color (Cpy ContextAdj' d) = text "(" <+> fmt_edge_type d <+> text ")"
fmt_context_adj end_color (Cpy NilAdjList' d) = fmt_context_node d
fmt_context_adj end_color (Del ContextAdj' d) = text "(" <+> fmt_edge_type d <+> text ")"
fmt_context_adj end_color (Del NilAdjList' d) = fmt_context_node d
fmt_context_adj end_color (Ins ContextAdj' d) = text "(" <+> fmt_edge_type d <+> text ")"
fmt_context_adj end_color (Ins NilAdjList' d) = fmt_context_node d
fmt_context_adj end_color (CpyTree d)         = fmt_context_node d
fmt_context_adj _ (Cpy c _) = error ("fmt_context_adj" ++ string c)

fmt_edge_id :: EditScriptL AppStateTupleFam t t1 -> Doc e
fmt_edge_id (Cpy (Int' i) d) = pretty i <+> text "," <+> fmt_adj_list_start d
fmt_edge_id (Cpy c _) = error ("fmt_edge_id" ++ (string c))

fmt_edge_type (Cpy UserCreatedImage' d)  = text "UserCreatedImage" <+> fmt_edge_id  d
fmt_edge_type (Cpy UserCreatedRating' d) = text "UserCreatedRating" <+> fmt_edge_id  d
fmt_edge_type (Cpy ImageInRating' d)     = text "ImageInRating" <+> fmt_edge_id  d
fmt_edge_type (Del UserCreatedImage' d)  = text "UserCreatedImage" <+> fmt_edge_id  d
fmt_edge_type (Del UserCreatedRating' d) = text "UserCreatedRating" <+> fmt_edge_id  d
fmt_edge_type (Del ImageInRating' d)     = text "ImageInRating" <+> fmt_edge_id  d
fmt_edge_type (Ins UserCreatedImage' d)  = text "UserCreatedImage" <+> fmt_edge_id  d
fmt_edge_type (Ins UserCreatedRating' d) = text "UserCreatedRating" <+> fmt_edge_id  d
fmt_edge_type (Ins ImageInRating' d)     = text "ImageInRating" <+> fmt_edge_id  d

fmt_context_node (Cpy (Int' i) d) = pretty i <+> fmt_node_type d

fmt_node_type (Cpy U' d) = fmt_user_node d
fmt_node_type (Cpy I' d) = fmt_image_node d
fmt_node_type (Cpy R' d) = fmt_rating_node d

fmt_user_node (Cpy UserNode' d) = text "UserNode" <+> fmt_email d

fmt_email (Cpy (String' s) d) = text "email =" <+> text s <+> fmt_facebook_id d 

fmt_facebook_id (Cpy (String' s) d) = text "facebook_id =" <+> text s <+> fmt_twitter_id d

fmt_twitter_id (Cpy (String' s) d) = text "twitter_id =" <+> text s </> text "[" <+> fmt_ios_device_tokens d

fmt_ios_device_tokens :: EditScriptL AppStateTupleFam t t1 -> Doc e
fmt_ios_device_tokens (Cpy (String' s)    d) = text s   <+> fmt_ios_device_tokens d
fmt_ios_device_tokens (Cpy NilStringList' d) = text "]" <+> fmt_end_adj_list d

fmt_end_adj_list (Cpy AdjList' d) = text "[" <+> fmt_end_adj_element d
fmt_end_adj_list (Cpy NilAdjList' d) = fmt_context_list_element d

fmt_end_adj_element (Cpy ContextAdj' d) = text "(" <+> fmt_edge_type d <+> text ")"
fmt_end_adj_element (Cpy NilAdjList' d) = fmt_context_list_element d



fmt_image_node = error "fmt_image_node"

fmt_rating_node = error "fmt_rating_node"



fmt_adj_list = error "=\\\\\\\\\\\\\\\\\\\\\\\\\\

]fmt_leaderboards = error "fmt_leaderboards"