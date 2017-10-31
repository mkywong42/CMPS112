#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Matthew Lo
 mamlo@ucsc.edu
 CMPS 112 asg3"

 "Name: hzip-st - file compression and decompression utility
  Synopsis: hzip.st -dtcu inputfile [outputfile]
  Exactly one option required.
  OPTIONS:
  -d Debug information is printed for the benefit of the application
     author. Exact details are not specified.

  -t The compression algorithm is activated, and the decoding
     tree is printed to the standard output. The output
     filename may not be specified.

  -c The input file is compressed and written to the output
     file, if specified, or to the standard output, if not.

  -u The input file is assumed to be compressed, and is
     uncompressed, written to the output file, if specified, or
     to the standard output, if not.
  "
nl := Character nl.
FileStream extend [
   nl := Character nl.
  " pqueue := SortedCollection new."
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

"-------------classes--------------"

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

"Leaf subclass modified from tree.st"
Object subclass: Leaf [
   |char count aleaf|
   char [ ^ char ]
   count [ ^ count ]
   aleaf [ ^ aleaf ]
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      aleaf := true. "is a leaf"
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   "need a post-order traversal"
   postorder: visitor [
      visitor value: char value: aleaf.
   ]
]

"Tree subclass of leaf modified from tree.st"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      aleaf := false. "trees are not leaves"
   ]

   right [ ^ right ]
   left [ ^ left ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor [
      left postorder: visitor. "left first"
      right postorder: visitor. "right next"
      visitor value: char value: aleaf.
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]
"*-------------------------------*"
|inputfile readfile outputfile dcpfile outfilename argc|

opD := false.
opT := false.
opC := false.
opU := false.
outfilename := stdout.
"infile := ''."
priorityqueue := SortedCollection new.

(Smalltalk arguments size > 1) & (Smalltalk arguments size < 4)
ifTrue: [
   "Smalltalk arguments do: [:argu |"
   (1 to: Smalltalk arguments size)
      do: [:index |
         argu := (Smalltalk arguments at: index).
      "if the argument has -"
      (argu includes: $-)
      ifTrue: [ "check d, t, c, u"
         (argu includes: $d) ifTrue: [ opD := true. ].
         (argu includes: $t) ifTrue: [ opT := true. ].
         (argu includes: $c) ifTrue: [ opC := true. ].
         (argu includes: $u) ifTrue: [ opU := true. ].
      ]
      "if not, check for infile"
      ifFalse: [
         (infile ~= nil) ifFalse: [
            infile := argu.
         ]
      "if infile has been found, check outfile"
         ifTrue: [
            outfilename := argu.
         ]
      ]
   ].
]
ifFalse: [
   stderr << 'error: Usage: hzip.st -dtcu inputfile [outputfile]' << nl.
   ObjectMemory quit: 1
].

opD ifTrue: [
   stdout << 'Debugger: input file is ' << infile << ' || '
   << 'output file is ' << outfilename << nl.
].

|inputfile writefile|
inputfile := FileStream open: infile
                        mode: FileStream read.


outfilename = stdout
ifTrue:[ writefile := nil. ]
ifFalse:[
   writefile := FileStream open: outfilename
                           mode: FileStream write.
].

freqtable := ZeroArray new: 257.
freqtable at:256 put:1.
         
"---------------Compression--------------"

opU ifFalse: [
encodingT := ZeroArray new: 257.
"part a"
[inputfile atEnd not] whileTrue: [
   |ordChar|
   ordChar := inputfile next asInteger.
   freqtable incr: ordChar.
].


"part b"
freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
      priorityqueue add: (Leaf new: key count: value).
   ]
].

"part c"
[priorityqueue size > 1] whileTrue: [
   |lchild rchild tCount tChar| 
   lchild := priorityqueue removeFirst.
   rchild := priorityqueue removeFirst.
   tChar := lchild char.
   tCount := (rchild count + lchild count).
   newTree := Tree new: tChar count: tCount left: lchild right: rchild.
   priorityqueue add: newTree.
].

"part d"
etree := priorityqueue removeFirst.
etree = nil ifFalse: [
   etree inorder: [:char :string |
      encodingT at: char put: string.
   ] prefix: ''.
].

"part e"
"if t is specified"
opT ifTrue: [
  |char num bitstring count temp|
   encodingT keysAndValuesDo: [:index :num |
      (num notNil and: [num > 0]) ifTrue: [
      
      ((index > 32) & (index < 128)) ifTrue: [
         char := (Character value: index).
      ]
      ifFalse: [
         index = 256 ifTrue: [char := 'EOF']
         ifFalse: [
            char:= (Character value: index) asInteger radix: 16.
            char := char copyFrom: 4 to: char size.
            index < 16 ifTrue:[
               char := ('x0', char).
            ]
            ifFalse: [
               index < 33 ifTrue:[ char := ('x', char). ]
            ]
         ]
      ].
      bitstring := encodingT at: index.
      count := 0. 
      stdout << char.
      char asString size to: 5 do: [:skip| stdout << ' '].
      stdout << (freqtable at: index).
      temp := (freqtable at: index). 
      temp asString size to: 4 do: [:skip| stdout << ' '].
      stdout << bitstring << nl.
      ].
   ].
].

opC ifTrue: [
   |readfile|
   readfile := FileStream open: infile
                          mode: FileStream read.
  
   opD ifTrue: [
      (writefile = nil) ifTrue: [
         stdout << 'Writefile is nil. Printing to stdout.' << nl.
      ]
      ifFalse:[
         stdout << 'Printing to ' << outfilename << nl.
      ].
      stdout << '-------Compression-------' << nl.
   ].
   writefile = nil
   "if there is no outfile"
   ifTrue: [
      writefile := stdout.
   ]
   ifFalse: [
      writefile := FileStream open: outfilename
                              mode: FileStream write.
   ].
   
   bitrep := ''.
   etree postorder: [:char :aleaf |
      aleaf ifTrue:[
         tempbit := ''.
         char = 0 "if byte is 0"
         ifTrue:[tempbit := '000000000']
         ifFalse:[
            char = 256 "if byte is EOF"
            ifTrue:[tempbit := '000000001']
            ifFalse:["if not 0 or 256, write from bit 7 to 0"
               8 to: 1 by: -1 do: [:i |
                  tempbit := tempbit, ((char bitAt: i) printString).
               ]
            ].
         ].
         bitrep := (bitrep, '0', tempbit).
      ]
      ifFalse:[ "if not leaf"
         bitrep := (bitrep, '1').
      ].
   ].
      "copy it into outfile"
      copybits := OutBits new: writefile.
      1 to: (bitrep size) do: [:x |
        bitcopy := ((bitrep at: x) asInteger) - 48.
        copybits writeBit: bitcopy.
      ].
      copybits writeBit: 1.
      "copy encoding value into outfile"
      "reopen the infile and write out the encoded version of each byte"
      [readfile atEnd not] whileTrue: [
         |index bitwise code|
         index := readfile next asInteger.
         code := encodingT at: index.
         code do: [:char |
            bitwise := char asInteger - 48.
            copybits writeBit: bitwise.
         ].
      ].
      eoftrue := encodingT at: 256.
      eoftrue do: [:char |
         bitwise := char asInteger - 48.
         copybits writeBit: bitwise.
      ].
      copybits flushByte. 
   ].
].

"-----------------------decompression--------------------------"

opU ifTrue: [
   |stack retree eof|
   "initialize stack"
   "copybits := OutBits new: writefile."
   
   opD ifTrue:[
       (writefile = nil) ifTrue:[
          stdout << 'Writefile is nil. Printing to stdout' << nl.
       ]
       ifFalse:[
          stdout << 'Printing to ' << outfilename << nl.
       ].
       stdout << '------Decompression------' << nl.
   ].
   writefile = nil
   ifTrue:[ writefile := stdout. ].
   stack := OrderedCollection new.
   bitsInFile := BitStream new: inputfile.
   eof := false.
   bitChar := 0.
   reTree := ''.
   rootnode := ''.
   finished := false.
   "rebuild the tree."
   [(bitsInFile atEnd not) & (eof not)] whileTrue:[
      bitChar := 0.
      oneBits := bitsInFile nextBit.

      finished ifFalse:[
        " oneBit := bitsInFile nextBit."
         "if the bit is 0, read the next 8 bits"
         oneBits = 0 ifTrue:[
            8 to: 1 by: -1 do: [:bitindex |
               oneBit := bitsInFile nextBit.
              bitChar := bitChar bitAt: bitindex put: oneBit.
            ].
            "if value is 0, read one more bit"
            bitChar = 0 ifTrue:[
               oneBit := bitsInFile nextBit.

            "if it is 1, add 256 to bitchar"
               oneBit = 1 ifTrue:[
                  bitChar := bitChar bitAt: 9 put: 1.
               ].
            ].
      
         stack add: (Leaf new: bitChar count: 0).
         ].

         "if the bit is 1"
         oneBits = 1 ifTrue:[
            righttr := stack removeLast.
            
            "if that was the last tree"
            stack size = 0 ifTrue:[
               reTree := righttr.
               rootnode := reTree.
               finished := true.
            ]
            ifFalse:[
               lefttr := stack removeLast.
               newTree := Tree new: 
               lefttr char count: 0 left: lefttr right: righttr.
               stack add: newTree.
            ].
         ].
      ]
      ifTrue: [ "if tree has been reconstructed"
      "part e"
         (rootnode aleaf) ifTrue:[

            "if we've found the leaf eof, end loop"
            rootnode char = 256 ifTrue: [eof := true]
            ifFalse:[
               echar := Character value: rootnode char.
               writefile nextPut: echar.
               rootnode := reTree.
            ].
         ]
         "if not a leaf"
         ifFalse: [
            oneBits = 0 ifTrue: [
               rootnode := rootnode left.
               rootnode aleaf ifTrue:[
                  eof not ifTrue:[
                     rootnode char = 256 ifTrue: [eof := true]
                  
                     ifFalse: [
                        echar := Character value: rootnode char.
                        writefile nextPut: echar.
                        rootnode := reTree.
                     ].
                  ]
               ]
            ]

            "if not 0, check right"
            ifFalse:[
               rootnode := rootnode right.
               rootnode aleaf ifTrue:[
                  rootnode char = 256 ifTrue: [eof := true]
                  ifFalse:[
                     echar := Character value: rootnode char.
                     writefile nextPut: echar.
                     rootnode := reTree.
                  ]
               ]
            ].
         ].
      ].
   ].
].
inputfile close.
writefile ~= nil ifTrue:[
   writefile close.
].
ObjectMemory quit: 0
