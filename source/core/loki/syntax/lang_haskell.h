/* lang_haskell.h - Haskell/MHS language definition */

#ifndef LOKI_LANG_HASKELL_H
#define LOKI_LANG_HASKELL_H

/* Haskell keywords */
static char *Haskell_HL_keywords[] = {
    /* Reserved words */
    "module","import","qualified","as","hiding",
    "where","let","in","case","of","if","then","else",
    "do","return","class","instance","data","type","newtype",
    "deriving","default","infix","infixl","infixr",
    /* Common functions */
    "main","print","putStrLn","putStr","show","read",
    "map","filter","foldr","foldl","zip","unzip",
    "head","tail","init","last","length","null","reverse",
    "take","drop","concat","sum","product","maximum","minimum",
    "and","or","not","otherwise",
    /* Monadic */
    "pure","fmap","bind","sequence","mapM","forM",
    /* MHS MIDI primitives */
    "midiInit","midiCleanup","midiListPorts","midiPortCount",
    "midiOpenPort","midiOpenVirtual","midiClose",
    "midiNoteOn","midiNoteOff","midiCC","midiProgramChange",
    "midiPitchBend","midiPanic","midiSleep",
    /* Music module */
    "note","rest","chord","line","times","tempo",
    "instrument","dynamic","phrase","cut","remove",
    /* Types (highlighted differently with | suffix) */
    "Int|","Integer|","Float|","Double|","Bool|","Char|","String|",
    "Maybe|","Either|","IO|","List|","Monad|","Functor|","Applicative|",
    "True|","False|","Just|","Nothing|","Left|","Right|",
    NULL
};

static char *Haskell_HL_extensions[] = {".hs",".mhs",".lhs",NULL};

#endif /* LOKI_LANG_HASKELL_H */
