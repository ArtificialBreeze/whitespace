library(compositions)
dfa.c<-setClass("dfa.c",
                slots=c(code="character",state="numeric",accumulator="list",buffer="character",stack="list",heap="environment",internal="environment"),
                prototype = c(code="",state=0,accumulator=c(),buffer="",stack=c(),heap=new.env(),internal=new.env()),
                validity=function(object)
                {
                  if(any(grepl("[^SLT]",object@code))) {
                    return("Non-whitespace symbol detected")
                  }
                  return(TRUE)
                }
)
setGeneric(name="parse",
           def=function(object)
           {
             standardGeneric("parse")
           }
)
setMethod(f="parse",
          signature="dfa.c",
          definition=function(object)
          {
            code=object@code
            state=if(length(object@state))object@state else 0
            accumulator=object@accumulator
            buffer=object@buffer
            i=1
            while(i<=length(code)){
              switch(toString(state),
                     "0"={
                       if(code[i]=="S")
                         state=1    # 1: ->STACK
                       else if(code[i]=="T"&code[i+1]=="S"){
                         i=i+1      # 2: ->ARITHMETIC
                         state=2
                       }
                       else if(code[i]=="T"&code[i+1]=="T"){
                         i=i+1
                         state=3    # 3: ->HEAP
                       }
                       else if(code[i]=="L"){
                         state=4    # 4: ->FLOW
                       }
                       else if(code[i]=="T"&code[i+1]=="L"){
                         i=i+1
                         state=5    # 5: ->I/O
                       }
                     },
                     "1"={
                       if(code[i]=="S"){
                         state=6    # 6: PUSH `pNUM`
                       }
                       else if(code[i]=="L"&code[i+1]=="S"){
                         accumulator=append(accumulator,"dup")
                         i=i+1
                         state=0     # 0: DUPLICATE Stack[upper]
                       }
                       else if(code[i]=="L"&code[i+1]=="T"){
                         accumulator=append(accumulator,"swap")
                         i=i+1
                         state=0     # 0: SWAP Stack[upper] Stack[lower]
                       }
                       else if(code[i]=="L"&code[i+1]=="L"){
                         accumulator=append(accumulator,"del")
                         i=i+1
                         state=0     # 0: DELETE Stack[upper]
                       }
                     },
                     "2"={
                       if(code[i]=="S"&code[i+1]=="S"){
                         i=i+1
                         state=0   # 0: Stack[lower] + Stack[upper]
                         accumulator=append(accumulator,"+")
                       }
                       else if(code[i]=="S"&code[i+1]=="T"){
                         i=i+1
                         state=0   # 0: Stack[lower] - Stack[upper]
                         accumulator=append(accumulator,"-")
                       }
                       else if(code[i]=="S"&code[i+1]=="L"){
                         i=i+1
                         state=0   # 0: `Stack[lower]` * `Stack[upper]`
                         accumulator=append(accumulator,"*")
                       }
                       else if(code[i]=="T"&code[i+1]=="S"){
                         i=i+1
                         state=0   # 0: `Stack[lower]` / `Stack[upper]`
                         accumulator=append(accumulator,"/")
                       }
                       else if(code[i]=="T"&code[i+1]=="T"){
                         i=i+1
                         state=0   # 0: `Stack[lower]` % `Stack[upper]`
                         accumulator=append(accumulator,"%")
                       }
                     },
                     "3"={
                       if(code[i]=="S"){
                         accumulator=append(accumulator,"store")
                         state=0    # 0: Stack[lower]=address Stack[upper]=value
                       }
                       if(code[i]=="T"){
                         accumulator=append(accumulator,"retrieve")
                         state=0    # 0: Stack[lower]=address Stack[upper]=returned value
                       }
                     },
                     "4"={
                       if(code[i]=="S"&code[i+1]=="S"){
                         i=i+1
                         buffer="mark "
                         state=11
                       }
                       else if(code[i]=="S"&code[i+1]=="T"){
                         i=i+1
                         buffer="call "
                         state=11
                       }
                       else if(code[i]=="S"&code[i+1]=="L"){
                         i=i+1
                         buffer="jp "
                         state=11
                       }
                       else if(code[i]=="T"&code[i+1]=="S"){
                         i=i+1
                         buffer="jz "
                         state=11
                       }
                       else if(code[i]=="T"&code[i+1]=="T"){
                         i=i+1
                         buffer="jn "
                         state=11
                       }
                       else if(code[i]=="T"&code[i+1]=="L"){
                         i=i+1
                         accumulator=append(accumulator,"return")
                         state=0
                       }
                       else if(code[i]=="L"&code[i+1]=="L"){
                         i=i+1
                         accumulator=append(accumulator,"end")
                         i=Inf
                       }
                     },
                     "5"={
                       if(code[i]=="S"&code[i+1]=="S"){
                         accumulator=append(accumulator,"printc")
                         i=i+1
                         state=0
                       }
                       else if(code[i]=="S"&code[i+1]=="T"){
                         accumulator=append(accumulator,"printn")
                         i=i+1
                         state=0
                       }
                       else if(code[i]=="T"&code[i+1]=="S"){
                         accumulator=append(accumulator,"getc")
                         i=i+1
                         state=0
                       }
                       else if(code[i]=="T"&code[i+1]=="T"){
                         accumulator=append(accumulator,"getn")
                         i=i+1
                         state=0
                       }
                     },
                     "6"={
                       if(code[i]=="T")
                         buffer=paste0(buffer,"-")
                       state=10
                     },
                     "10"={
                       if(code[i]=="L")
                       {
                         x=gsub("S","0",gsub("T","1",buffer))
                         accumulator=append(accumulator,paste0(ifelse(grepl("-",x),"-",""),toString(unbinary(paste0(regmatches(x,gregexpr("[01]",x))[[1]],collapse = "")))))
                         buffer=""
                         state=0
                       }
                       else
                         buffer=paste0(buffer,code[i])
                     },
                     "11"={
                       if(code[i]=="L")
                       {
                         x=gsub("S","0",gsub("T","1",buffer))
                         accumulator=append(accumulator,buffer)
                         buffer=""
                         state=0
                       }
                       else
                         buffer=paste0(buffer,code[i])
                     })
              i=i+1
            }
            object@code=code
            object@accumulator=accumulator
            object@state=state
            validObject(object)
            return(object)
          }
)
setGeneric(name="compile",
           def=function(object)
           {
             standardGeneric("compile")
           }
)
setMethod(f="compile",
          signature="dfa.c",
          definition=function(object)
          {
            toChar=function(x)rawToChar(as.raw(x))
            accumulator=object@accumulator
            stack=object@stack
            heap=object@heap
            internal=object@internal
            i=1
            start=-1
            while(i<=length(accumulator)){ # x : instruction
              x=accumulator[[i]]
              if(!grepl("[^0-9-]",x)){
                stack<-append(x,stack)
              }
              else if(x=="dup"){
                u=head(stack,1)
                stack<-append(head(u,1),u)
              }
              else if(x=="swap"){
                stack<-append(c(stack[[2]],stack[[1]]),tail(stack,-2))
              }
              else if(x=="del"){
                stack<-tail(stack,-1)
              }
              else if(x=="+"){
                stack<-append(toString(as.numeric(stack[[2]])+as.numeric(stack[[1]])),tail(stack,-2))
              }
              else if(x=="-"){
                stack<-append(toString(as.numeric(stack[[2]])-as.numeric(stack[[1]])),tail(stack,-2))
              }
              else if(x=="*"){
                stack<-append(toString(as.numeric(stack[[2]])*as.numeric(stack[[1]])),tail(stack,-2))
              }
              else if(x=="/"){
                stack<-append(toString(as.numeric(stack[[2]])/as.numeric(stack[[1]])),tail(stack,-2))
              }
              else if(x=="%"){
                stack<-append(toString(as.numeric(stack[[2]])%%as.numeric(stack[[1]])),tail(stack,-2))
              }
              else if(x=="store"){
                assign(stack[[2]], stack[[1]], envir=heap)
                stack<-tail(stack,-2)
              }
              else if(x=="retrieve"){
                value=get(stack[[1]],envir=heap)
                stack<-append(value,tail(stack,-1))
              }
              else if(grepl("mark",x)){
                value=regmatches(x,gregexpr("[ST]+",x))[[1]]
                assign(value,i,envir=internal)
              }
              else if(grepl("call",x)){
                label=regmatches(x,gregexpr("[ST]+",x))[[1]]
                start=i+1
                i=get(label,envir=internal)
                next
              }
              else if(grepl("jp",x)){
                label=regmatches(x,gregexpr("[ST]+",x))[[1]]
                i=get(label,envir=internal)
                next
              }
              else if(grepl("jz",x)){
                label=regmatches(x,gregexpr("[ST]+",x))[[1]]
                cond=as.numeric(stack[[1]])
                if(!cond){
                  i=get(label,envir=internal)
                  next
                }
              }
              else if(grepl("jn",x)){
                label=regmatches(x,gregexpr("[ST]+",x))[[1]]
                cond=as.numeric(stack[[1]])
                if(cond<0){
                  i=get(label,envir=internal)
                  next
                }
              }
              else if(x=="return"){
                if(start<0)
                  stop("Error: Nothing to return")
                i=start
                next
              }
              else if(x=="end"){
                cat("\nProgram stop")
              }
              else if(x=="printc"){
                cat("Program output: ",toChar(stack[[1]]))
              }
              else if(x=="printn"){
                cat("\nProgram output: ",stack[[1]])
              }
              else if(x=="getc"){
                n=readline(prompt="\nInput a char: ")
                if(is.character(n)&nchar(n)==1){
                  assign(stack[[1]],n, envir=heap)
                }
                else{
                  stop("Error: Not a character")
                }
              }
              else if(x=="getn"){
                n=as.integer(readline(prompt="\nInput a number: "))
                if(grepl("^[0-9]+$",n))
                {
                  assign(stack[[1]],n, envir=heap)
                }
                else{
                  stop("Error: Not a number")
                }
              }
              i=i+1
            }
            object@internal=internal
            object@heap=heap
            object@stack=stack
            validObject(object)
            return(object)
          }
)
dfa <- function(code) dfa.c(code=strsplit(gsub("[^STL]","",gsub("\t","T",gsub("\n","L",gsub(" ","S",code)))),"")[[1]])
