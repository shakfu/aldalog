/* lang_bog.h - Bog (Prolog-based music) language definition */

#ifndef LOKI_LANG_BOG_H
#define LOKI_LANG_BOG_H

/* Bog keywords - predicates and built-ins */
static char *Bog_HL_keywords[] = {
    /* Core event predicate */
    "event",
    /* Timing predicates */
    "every","beat","phase","euc",
    /* Selection predicates */
    "choose","pick","cycle","rand","randint","prob",
    /* Comparison predicates */
    "eq","lt","gt","lte","gte","within","distinct",
    /* Music theory */
    "scale","chord","transpose","add","range","rotate",
    /* Utility */
    "cooldown","is",
    /* Prolog operators */
    ":-","true","false","fail",
    /* Scale names (types) */
    "ionian|","dorian|","phrygian|","lydian|","mixolydian|",
    "aeolian|","locrian|","major_pent|","minor_pent|","blues|",
    /* Chord names (types) */
    "maj|","min|","sus2|","sus4|","dim|","aug|","maj7|","dom7|","min7|",
    /* Voice names (types) */
    "kick|","snare|","hat|","clap|","noise|",
    "sine|","square|","triangle|","saw|",
    NULL
};

static char *Bog_HL_extensions[] = {".bog",".pl",NULL};

#endif /* LOKI_LANG_BOG_H */
