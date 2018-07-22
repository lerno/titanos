//
//  main.c
//  titanos
//
//  Created by Christoffer Lerno on 2018-07-21.
//  Copyright © 2018 Christoffer Lerno. All rights reserved.
//

#include <stdio.h>
#include "file.h"
#include "lexer.h"
#include "debug.h"
#include "parser.h"
int main(int argc, const char * argv[]) {
    // insert code here...
    
    char *foo = read_file("/Users/lerno/Projects/titanos/titanos/test.ti");
    parse(foo);
    return 0;
}


