-record(post, {id, title, date, keywords, body}).
-record(comment, 
	{id, post, date, email, nick, title, body, type}).
