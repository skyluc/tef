# TExt Format

    tef := element* comment* lws*
    
    element := elementDecl attribute* element* elementEnd
    
    attribute := comment* lws '.' ws name ws value
    
    elementDecl := comment* lws '+' ws name ws value
    
    elementEnd := lws '-' wp nl
    
    name := [^ ws nl ]*
    
    value := [^ nl ]* nl addValue*
    
    addValue := lws '=' [^ nl ]* nl
    
    comment := lws '#' [^ nl ]* nl
    
    lws := wl* ws    
    
    wl := ws nl
    
    ws := [ ' ' '\t' ]*
    
    nl := '\n'