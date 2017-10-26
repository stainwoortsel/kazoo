Options = [{startkey,[<<"sdsdsd">>,4343]}
          ,{endkey,[<<"sdsdsd">>,3445,{[]}]}
          ,{group,true}
          ,{group_level,3}
          ,{reduce,true}
          ],

kz_fixtures_util:encode_query_filename(<<"cdrs/interaction_listing_by_owner">>, Options).

"cdrs+interaction_listing_by_owner-1ac4e403c3ce1ba72c68832141d86464.json"
