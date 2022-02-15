const WORDS = ["aback", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abled", "abode", "abort",
    "about", "above", "abuse", "abyss", "acorn", "acrid", "actor", "acute", "adage", "adapt",
    "adept", "admin", "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire",
    "afoot", "afoul", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow",
    "agony", "agree", "ahead", "aider", "aisle", "alarm", "album", "alert", "algae", "alibi",
    "alien", "align", "alike", "alive", "allay", "alley", "allot", "allow", "alloy", "aloft",
    "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", "amass", "amaze", "amber",
    "amble", "amend", "amiss", "amity", "among", "ample", "amply", "amuse", "angel", "anger",
    "angle", "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "anode", "antic",
    "anvil", "aorta", "apart", "aphid", "aping", "apnea", "apple", "apply", "apron", "aptly",
    "arbor", "ardor", "arena", "argue", "arise", "armor", "aroma", "arose", "array", "arrow",
    "arson", "artsy", "ascot", "ashen", "aside", "askew", "assay", "asset", "atoll", "atone",
    "attic", "audio", "audit", "augur", "aunty", "avail", "avert", "avian", "avoid", "await",
    "awake", "award", "aware", "awash", "awful", "awoke", "axial", "axiom", "axion", "azure",
    "bacon", "badge", "badly", "bagel", "baggy", "baker", "baler", "balmy", "banal", "banjo",
    "barge", "baron", "basal", "basic", "basil", "basin", "basis", "baste", "batch", "bathe",
    "baton", "batty", "bawdy", "bayou", "beach", "beady", "beard", "beast", "beech", "beefy",
    "befit", "began", "begat", "beget", "begin", "begun", "being", "belch", "belie", "belle",
    "belly", "below", "bench", "beret", "berry", "berth", "beset", "betel", "bevel", "bezel",
    "bible", "bicep", "biddy", "bigot", "bilge", "billy", "binge", "bingo", "biome", "birch",
    "birth", "bison", "bitty", "black", "blade", "blame", "bland", "blank", "blare", "blast",
    "blaze", "bleak", "bleat", "bleed", "bleep", "blend", "bless", "blimp", "blind", "blink",
    "bliss", "blitz", "bloat", "block", "bloke", "blond", "blood", "bloom", "blown", "bluer",
    "bluff", "blunt", "blurb", "blurt", "blush", "board", "boast", "bobby", "boney", "bongo",
    "bonus", "booby", "boost", "booth", "booty", "booze", "boozy", "borax", "borne", "bosom",
    "bossy", "botch", "bough", "boule", "bound", "bowel", "boxer", "brace", "braid", "brain",
    "brake", "brand", "brash", "brass", "brave", "bravo", "brawl", "brawn", "bread", "break",
    "breed", "briar", "bribe", "brick", "bride", "brief", "brine", "bring", "brink", "briny",
    "brisk", "broad", "broil", "broke", "brood", "brook", "broom", "broth", "brown", "brunt",
    "brush", "brute", "buddy", "budge", "buggy", "bugle", "build", "built", "bulge", "bulky",
    "bully", "bunch", "bunny", "burly", "burnt", "burst", "bused", "bushy", "butch", "butte",
    "buxom", "buyer", "bylaw", "cabal", "cabby", "cabin", "cable", "cacao", "cache", "cacti",
    "caddy", "cadet", "cagey", "cairn", "camel", "cameo", "canal", "candy", "canny", "canoe",
    "canon", "caper", "caput", "carat", "cargo", "carol", "carry", "carve", "caste", "catch",
    "cater", "catty", "caulk", "cause", "cavil", "cease", "cedar", "cello", "chafe", "chaff",
    "chain", "chair", "chalk", "champ", "chant", "chaos", "chard", "charm", "chart", "chase",
    "chasm", "cheap", "cheat", "check", "cheek", "cheer", "chess", "chest", "chick", "chide",
    "chief", "child", "chili", "chill", "chime", "china", "chirp", "chock", "choir", "choke",
    "chord", "chore", "chose", "chuck", "chump", "chunk", "churn", "chute", "cider", "cigar",
    "cinch", "circa", "civic", "civil", "clack", "claim", "clamp", "clang", "clank", "clash",
    "clasp", "class", "clean", "clear", "cleat", "cleft", "clerk", "click", "cliff", "climb",
    "cling", "clink", "cloak", "clock", "clone", "close", "cloth", "cloud", "clout", "clove",
    "clown", "cluck", "clued", "clump", "clung", "coach", "coast", "cobra", "cocoa", "colon",
    "color", "comet", "comfy", "comic", "comma", "conch", "condo", "conic", "copse", "coral",
    "corer", "corny", "couch", "cough", "could", "count", "coupe", "court", "coven", "cover",
    "covet", "covey", "cower", "coyly", "crack", "craft", "cramp", "crane", "crank", "crash",
    "crass", "crate", "crave", "crawl", "craze", "crazy", "creak", "cream", "credo", "creed",
    "creek", "creep", "creme", "crepe", "crept", "cress", "crest", "crick", "cried", "crier",
    "crime", "crimp", "crisp", "croak", "crock", "crone", "crony", "crook", "cross", "croup",
    "crowd", "crown", "crude", "cruel", "crumb", "crump", "crush", "crust", "crypt", "cubic",
    "cumin", "curio", "curly", "curry", "curse", "curve", "curvy", "cutie", "cyber", "cycle",
    "cynic", "daddy", "daily", "dairy", "daisy", "dally", "dance", "dandy", "datum", "daunt",
    "dealt", "death", "debar", "debit", "debug", "debut", "decal", "decay", "decor", "decoy",
    "decry", "defer", "deign", "deity", "delay", "delta", "delve", "demon", "demur", "denim",
    "dense", "depot", "depth", "derby", "deter", "detox", "deuce", "devil", "diary", "dicey",
    "digit", "dilly", "dimly", "diner", "dingo", "dingy", "diode", "dirge", "dirty", "disco",
    "ditch", "ditto", "ditty", "diver", "dizzy", "dodge", "dodgy", "dogma", "doing", "dolly",
    "donor", "donut", "dopey", "doubt", "dough", "dowdy", "dowel", "downy", "dowry", "dozen",
    "draft", "drain", "drake", "drama", "drank", "drape", "drawl", "drawn", "dread", "dream",
    "dress", "dried", "drier", "drift", "drill", "drink", "drive", "droit", "droll", "drone",
    "drool", "droop", "dross", "drove", "drown", "druid", "drunk", "dryer", "dryly", "duchy",
    "dully", "dummy", "dumpy", "dunce", "dusky", "dusty", "dutch", "duvet", "dwarf", "dwell",
    "dwelt", "dying", "eager", "eagle", "early", "earth", "easel", "eaten", "eater", "ebony",
    "eclat", "edict", "edify", "eerie", "egret", "eight", "eject", "eking", "elate", "elbow",
    "elder", "elect", "elegy", "elfin", "elide", "elite", "elope", "elude", "email", "embed",
    "ember", "emcee", "empty", "enact", "endow", "enema", "enemy", "enjoy", "ennui", "ensue",
    "enter", "entry", "envoy", "epoch", "epoxy", "equal", "equip", "erase", "erect", "erode",
    "error", "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude", "evade", "event",
    "every", "evict", "evoke", "exact", "exalt", "excel", "exert", "exile", "exist", "expel",
    "extol", "extra", "exult", "eying", "fable", "facet", "faint", "fairy", "faith", "false",
    "fancy", "fanny", "farce", "fatal", "fatty", "fault", "fauna", "favor", "feast", "fecal",
    "feign", "fella", "felon", "femme", "femur", "fence", "feral", "ferry", "fetal", "fetch",
    "fetid", "fetus", "fever", "fewer", "fiber", "ficus", "field", "fiend", "fiery", "fifth",
    "fifty", "fight", "filer", "filet", "filly", "filmy", "filth", "final", "finch", "finer",
    "first", "fishy", "fixer", "fizzy", "fjord", "flack", "flail", "flair", "flake", "flaky",
    "flame", "flank", "flare", "flash", "flask", "fleck", "fleet", "flesh", "flick", "flier",
    "fling", "flint", "flirt", "float", "flock", "flood", "floor", "flora", "floss", "flour",
    "flout", "flown", "fluff", "fluid", "fluke", "flume", "flung", "flunk", "flush", "flute",
    "flyer", "foamy", "focal", "focus", "foggy", "foist", "folio", "folly", "foray", "force",
    "forge", "forgo", "forte", "forth", "forty", "forum", "found", "foyer", "frail", "frame",
    "frank", "fraud", "freak", "freed", "freer", "fresh", "friar", "fried", "frill", "frisk",
    "fritz", "frock", "frond", "front", "frost", "froth", "frown", "froze", "fruit", "fudge",
    "fugue", "fully", "fungi", "funky", "funny", "furor", "furry", "fussy", "fuzzy", "gaffe",
    "gaily", "gamer", "gamma", "gamut", "gassy", "gaudy", "gauge", "gaunt", "gauze", "gavel",
    "gawky", "gayer", "gayly", "gazer", "gecko", "geeky", "geese", "genie", "genre", "ghost",
    "ghoul", "giant", "giddy", "gipsy", "girly", "girth", "given", "giver", "glade", "gland",
    "glare", "glass", "glaze", "gleam", "glean", "glide", "glint", "gloat", "globe", "gloom",
    "glory", "gloss", "glove", "glyph", "gnash", "gnome", "godly", "going", "golem", "golly",
    "gonad", "goner", "goody", "gooey", "goofy", "goose", "gorge", "gouge", "gourd", "grace",
    "grade", "graft", "grail", "grain", "grand", "grant", "grape", "graph", "grasp", "grass",
    "grate", "grave", "gravy", "graze", "great", "greed", "green", "greet", "grief", "grill",
    "grime", "grimy", "grind", "gripe", "groan", "groin", "groom", "grope", "gross", "group",
    "grout", "grove", "growl", "grown", "gruel", "gruff", "grunt", "guard", "guava", "guess",
    "guest", "guide", "guild", "guile", "guilt", "guise", "gulch", "gully", "gumbo", "gummy",
    "guppy", "gusto", "gusty", "gypsy", "habit", "hairy", "halve", "handy", "happy", "hardy",
    "harem", "harpy", "harry", "harsh", "haste", "hasty", "hatch", "hater", "haunt", "haute",
    "haven", "havoc", "hazel", "heady", "heard", "heart", "heath", "heave", "heavy", "hedge",
    "hefty", "heist", "helix", "hello", "hence", "heron", "hilly", "hinge", "hippo", "hippy",
    "hitch", "hoard", "hobby", "hoist", "holly", "homer", "honey", "honor", "horde", "horny",
    "horse", "hotel", "hotly", "hound", "house", "hovel", "hover", "howdy", "human", "humid",
    "humor", "humph", "humus", "hunch", "hunky", "hurry", "husky", "hussy", "hutch", "hydro",
    "hyena", "hymen", "hyper", "icily", "icing", "ideal", "idiom", "idiot", "idler", "idyll",
    "igloo", "iliac", "image", "imbue", "impel", "imply", "inane", "inbox", "incur", "index",
    "inept", "inert", "infer", "ingot", "inlay", "inlet", "inner", "input", "inter", "intro",
    "ionic", "irate", "irony", "islet", "issue", "itchy", "ivory", "jaunt", "jazzy", "jelly",
    "jerky", "jetty", "jewel", "jiffy", "joint", "joist", "joker", "jolly", "joust", "judge",
    "juice", "juicy", "jumbo", "jumpy", "junta", "junto", "juror", "kappa", "karma", "kayak",
    "kebab", "khaki", "kinky", "kiosk", "kitty", "knack", "knave", "knead", "kneed", "kneel",
    "knelt", "knife", "knock", "knoll", "known", "koala", "krill", "label", "labor", "laden",
    "ladle", "lager", "lance", "lanky", "lapel", "lapse", "large", "larva", "lasso", "latch",
    "later", "lathe", "latte", "laugh", "layer", "leach", "leafy", "leaky", "leant", "leapt",
    "learn", "lease", "leash", "least", "leave", "ledge", "leech", "leery", "lefty", "legal",
    "leggy", "lemon", "lemur", "leper", "level", "lever", "libel", "liege", "light", "liken",
    "lilac", "limbo", "limit", "linen", "liner", "lingo", "lipid", "lithe", "liver", "livid",
    "llama", "loamy", "loath", "lobby", "local", "locus", "lodge", "lofty", "logic", "login",
    "loopy", "loose", "lorry", "loser", "louse", "lousy", "lover", "lower", "lowly", "loyal",
    "lucid", "lucky", "lumen", "lumpy", "lunar", "lunch", "lunge", "lupus", "lurch", "lurid",
    "lusty", "lying", "lymph", "lyric", "macaw", "macho", "macro", "madam", "madly", "mafia",
    "magic", "magma", "maize", "major", "maker", "mambo", "mamma", "mammy", "manga", "mange",
    "mango", "mangy", "mania", "manic", "manly", "manor", "maple", "march", "marry", "marsh",
    "mason", "masse", "match", "matey", "mauve", "maxim", "maybe", "mayor", "mealy", "meant",
    "meaty", "mecca", "medal", "media", "medic", "melee", "melon", "mercy", "merge", "merit",
    "merry", "metal", "meter", "metro", "micro", "midge", "midst", "might", "milky", "mimic",
    "mince", "miner", "minim", "minor", "minty", "minus", "mirth", "miser", "missy", "mocha",
    "modal", "model", "modem", "mogul", "moist", "molar", "moldy", "money", "month", "moody",
    "moose", "moral", "moron", "morph", "mossy", "motel", "motif", "motor", "motto", "moult",
    "mound", "mount", "mourn", "mouse", "mouth", "mover", "movie", "mower", "mucky", "mucus",
    "muddy", "mulch", "mummy", "munch", "mural", "murky", "mushy", "music", "musky", "musty",
    "myrrh", "nadir", "naive", "nanny", "nasal", "nasty", "natal", "naval", "navel", "needy",
    "neigh", "nerdy", "nerve", "never", "newer", "newly", "nicer", "niche", "niece", "night",
    "ninja", "ninny", "ninth", "noble", "nobly", "noise", "noisy", "nomad", "noose", "north",
    "nosey", "notch", "novel", "nudge", "nurse", "nutty", "nylon", "nymph", "oaken", "obese",
    "occur", "ocean", "octal", "octet", "odder", "oddly", "offal", "offer", "often", "olden",
    "older", "olive", "ombre", "omega", "onion", "onset", "opera", "opine", "opium", "optic",
    "orbit", "order", "organ", "other", "otter", "ought", "ounce", "outdo", "outer", "outgo",
    "ovary", "ovate", "overt", "ovine", "ovoid", "owing", "owner", "oxide", "ozone", "paddy",
    "pagan", "paint", "paler", "palsy", "panel", "panic", "pansy", "papal", "paper", "parer",
    "parka", "parry", "parse", "party", "pasta", "paste", "pasty", "patch", "patio", "patsy",
    "patty", "pause", "payee", "payer", "peace", "peach", "pearl", "pecan", "pedal", "penal",
    "pence", "penne", "penny", "perch", "peril", "perky", "pesky", "pesto", "petal", "petty",
    "phase", "phone", "phony", "photo", "piano", "picky", "piece", "piety", "piggy", "pilot",
    "pinch", "piney", "pinky", "pinto", "piper", "pique", "pitch", "pithy", "pivot", "pixel",
    "pixie", "pizza", "place", "plaid", "plain", "plait", "plane", "plank", "plant", "plate",
    "plaza", "plead", "pleat", "plied", "plier", "pluck", "plumb", "plume", "plump", "plunk",
    "plush", "poesy", "point", "poise", "poker", "polar", "polka", "polyp", "pooch", "poppy",
    "porch", "poser", "posit", "posse", "pouch", "pound", "pouty", "power", "prank", "prawn",
    "preen", "press", "price", "prick", "pride", "pried", "prime", "primo", "print", "prior",
    "prism", "privy", "prize", "probe", "prone", "prong", "proof", "prose", "proud", "prove",
    "prowl", "proxy", "prude", "prune", "psalm", "pubic", "pudgy", "puffy", "pulpy", "pulse",
    "punch", "pupil", "puppy", "puree", "purer", "purge", "purse", "pushy", "putty", "pygmy",
    "quack", "quail", "quake", "qualm", "quark", "quart", "quash", "quasi", "queen", "queer",
    "quell", "query", "quest", "queue", "quick", "quiet", "quill", "quilt", "quirk", "quite",
    "quota", "quote", "quoth", "rabbi", "rabid", "racer", "radar", "radii", "radio", "rainy",
    "raise", "rajah", "rally", "ralph", "ramen", "ranch", "randy", "range", "rapid", "rarer",
    "raspy", "ratio", "ratty", "raven", "rayon", "razor", "reach", "react", "ready", "realm",
    "rearm", "rebar", "rebel", "rebus", "rebut", "recap", "recur", "recut", "reedy", "refer",
    "refit", "regal", "rehab", "reign", "relax", "relay", "relic", "remit", "renal", "renew",
    "repay", "repel", "reply", "rerun", "reset", "resin", "retch", "retro", "retry", "reuse",
    "revel", "revue", "rhino", "rhyme", "rider", "ridge", "rifle", "right", "rigid", "rigor",
    "rinse", "ripen", "riper", "risen", "riser", "risky", "rival", "river", "rivet", "roach",
    "roast", "robin", "robot", "rocky", "rodeo", "roger", "rogue", "roomy", "roost", "rotor",
    "rouge", "rough", "round", "rouse", "route", "rover", "rowdy", "rower", "royal", "ruddy",
    "ruder", "rugby", "ruler", "rumba", "rumor", "rupee", "rural", "rusty", "sadly", "safer",
    "saint", "salad", "sally", "salon", "salsa", "salty", "salve", "salvo", "sandy", "saner",
    "sappy", "sassy", "satin", "satyr", "sauce", "saucy", "sauna", "saute", "savor", "savoy",
    "savvy", "scald", "scale", "scalp", "scaly", "scamp", "scant", "scare", "scarf", "scary",
    "scene", "scent", "scion", "scoff", "scold", "scone", "scoop", "scope", "score", "scorn",
    "scour", "scout", "scowl", "scram", "scrap", "scree", "screw", "scrub", "scrum", "scuba",
    "sedan", "seedy", "segue", "seize", "semen", "sense", "sepia", "serif", "serum", "serve",
    "setup", "seven", "sever", "sewer", "shack", "shade", "shady", "shaft", "shake", "shaky",
    "shale", "shall", "shalt", "shame", "shank", "shape", "shard", "share", "shark", "sharp",
    "shave", "shawl", "shear", "sheen", "sheep", "sheer", "sheet", "sheik", "shelf", "shell",
    "shied", "shift", "shine", "shiny", "shire", "shirk", "shirt", "shoal", "shock", "shone",
    "shook", "shoot", "shore", "shorn", "short", "shout", "shove", "shown", "showy", "shrew",
    "shrub", "shrug", "shuck", "shunt", "shush", "shyly", "siege", "sieve", "sight", "sigma",
    "silky", "silly", "since", "sinew", "singe", "siren", "sissy", "sixth", "sixty", "skate",
    "skier", "skiff", "skill", "skimp", "skirt", "skulk", "skull", "skunk", "slack", "slain",
    "slang", "slant", "slash", "slate", "sleek", "sleep", "sleet", "slept", "slice", "slick",
    "slide", "slime", "slimy", "sling", "slink", "sloop", "slope", "slosh", "sloth", "slump",
    "slung", "slunk", "slurp", "slush", "slyly", "smack", "small", "smart", "smash", "smear",
    "smell", "smelt", "smile", "smirk", "smite", "smith", "smock", "smoke", "smoky", "smote",
    "snack", "snail", "snake", "snaky", "snare", "snarl", "sneak", "sneer", "snide", "sniff",
    "snipe", "snoop", "snore", "snort", "snout", "snowy", "snuck", "snuff", "soapy", "sober",
    "soggy", "solar", "solid", "solve", "sonar", "sonic", "sooth", "sooty", "sorry", "sound",
    "south", "sower", "space", "spade", "spank", "spare", "spark", "spasm", "spawn", "speak",
    "spear", "speck", "speed", "spell", "spelt", "spend", "spent", "sperm", "spice", "spicy",
    "spied", "spiel", "spike", "spiky", "spill", "spilt", "spine", "spiny", "spire", "spite",
    "splat", "split", "spoil", "spoke", "spoof", "spook", "spool", "spoon", "spore", "sport",
    "spout", "spray", "spree", "sprig", "spunk", "spurn", "spurt", "squad", "squat", "squib",
    "stack", "staff", "stage", "staid", "stain", "stair", "stake", "stale", "stalk", "stall",
    "stamp", "stand", "stank", "stare", "stark", "start", "stash", "state", "stave", "stead",
    "steak", "steal", "steam", "steed", "steel", "steep", "steer", "stein", "stern", "stick",
    "stiff", "still", "stilt", "sting", "stink", "stint", "stock", "stoic", "stoke", "stole",
    "stomp", "stone", "stony", "stood", "stool", "stoop", "store", "stork", "storm", "story",
    "stout", "stove", "strap", "straw", "stray", "strip", "strut", "stuck", "study", "stuff",
    "stump", "stung", "stunk", "stunt", "style", "suave", "sugar", "suing", "suite", "sulky",
    "sully", "sumac", "sunny", "super", "surer", "surge", "surly", "sushi", "swami", "swamp",
    "swarm", "swash", "swath", "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift",
    "swill", "swine", "swing", "swirl", "swish", "swoon", "swoop", "sword", "swore", "sworn",
    "swung", "synod", "syrup", "tabby", "table", "taboo", "tacit", "tacky", "taffy", "taint",
    "taken", "taker", "tally", "talon", "tamer", "tango", "tangy", "taper", "tapir", "tardy",
    "tarot", "taste", "tasty", "tatty", "taunt", "tawny", "teach", "teary", "tease", "teddy",
    "teeth", "tempo", "tenet", "tenor", "tense", "tenth", "tepee", "tepid", "terra", "terse",
    "testy", "thank", "theft", "their", "theme", "there", "these", "theta", "thick", "thief",
    "thigh", "thing", "think", "third", "thong", "thorn", "those", "three", "threw", "throb",
    "throw", "thrum", "thumb", "thump", "thyme", "tiara", "tibia", "tidal", "tiger", "tight",
    "tilde", "timer", "timid", "tipsy", "titan", "tithe", "title", "toast", "today", "toddy",
    "token", "tonal", "tonga", "tonic", "tooth", "topaz", "topic", "torch", "torso", "torus",
    "total", "totem", "touch", "tough", "towel", "tower", "toxic", "toxin", "trace", "track",
    "tract", "trade", "trail", "train", "trait", "tramp", "trash", "trawl", "tread", "treat",
    "trend", "triad", "trial", "tribe", "trice", "trick", "tried", "tripe", "trite", "troll",
    "troop", "trope", "trout", "trove", "truce", "truck", "truer", "truly", "trump", "trunk",
    "truss", "trust", "truth", "tryst", "tubal", "tuber", "tulip", "tulle", "tumor", "tunic",
    "turbo", "tutor", "twang", "tweak", "tweed", "tweet", "twice", "twine", "twirl", "twist",
    "twixt", "tying", "udder", "ulcer", "ultra", "umbra", "uncle", "uncut", "under", "undid",
    "undue", "unfed", "unfit", "unify", "union", "unite", "unity", "unlit", "unmet", "unset",
    "untie", "until", "unwed", "unzip", "upper", "upset", "urban", "urine", "usage", "usher",
    "using", "usual", "usurp", "utile", "utter", "vague", "valet", "valid", "valor", "value",
    "valve", "vapid", "vapor", "vault", "vaunt", "vegan", "venom", "venue", "verge", "verse",
    "verso", "verve", "vicar", "video", "vigil", "vigor", "villa", "vinyl", "viola", "viper",
    "viral", "virus", "visit", "visor", "vista", "vital", "vivid", "vixen", "vocal", "vodka",
    "vogue", "voice", "voila", "vomit", "voter", "vouch", "vowel", "vying", "wacky", "wafer",
    "wager", "wagon", "waist", "waive", "waltz", "warty", "waste", "watch", "water", "waver",
    "waxen", "weary", "weave", "wedge", "weedy", "weigh", "weird", "welch", "welsh", "whack",
    "whale", "wharf", "wheat", "wheel", "whelp", "where", "which", "whiff", "while", "whine",
    "whiny", "whirl", "whisk", "white", "whole", "whoop", "whose", "widen", "wider", "widow",
    "width", "wield", "wight", "willy", "wimpy", "wince", "winch", "windy", "wiser", "wispy",
    "witch", "witty", "woken", "woman", "women", "woody", "wooer", "wooly", "woozy", "wordy",
    "world", "worry", "worse", "worst", "worth", "would", "wound", "woven", "wrack", "wrath",
    "wreak", "wreck", "wrest", "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly",
    "yacht", "yearn", "yeast", "yield", "young", "youth", "zebra", "zesty", "zonal"];


const letters = "abcdefghijklmnopqrstuvwxyz"
let word_dicts = [
    {}, {}, {}, {}, {}
]
let letter_dict = {}
for (let letter of letters) {
    letter_dict[letter] = []
    for (let index = 0; index < 5; index++) {
        word_dicts[index][letter] = []
    }
}

for (let word of WORDS) {
    for (let index = 0; index < 5; index++) {
        if (word[index] in word_dicts[index]) {
            word_dicts[index][word[index]].push(word)
        }
        letter_dict[word[index]].push(word)
    }
}

class Solver {
    constructor() {
        this.correct = ["", "", "", "", ""]
        this.exist = [[], [], [], [], []]
        this.incorrect = []
        this.guesses = 0
        this.possible_words = []
        this.min_index = -1
    }

    process_word(word, correct_indexes, exist_indexes) {
        for (let index = 0; index < 5; index++) {
            if (correct_indexes.indexOf(index) >= 0) {
                this.correct[index] = word[index]
            } else if (exist_indexes.indexOf(index) >= 0) {
                this.exist[index].push(word[index])
            } else if (this.incorrect.indexOf(word[index]) < 0) {
                this.incorrect.push(word[index])
            }
        }
    }

    process_possible_words() {
        let unfiltered_words
        if (this.guesses == 0) {
            unfiltered_words = this.initialize_words()
        } else {
            unfiltered_words = this.possible_words[this.guesses - 1]
        }
        let filtered_words = this.filterWords(unfiltered_words)
        this.possible_words.push(filtered_words)
        this.guesses++
    }

    initialize_words() {
        let words = this.get_shortest_words()
        if (words === null) {
            let exist_is_empty = true
            for (let index = 0; index < 5; index++) {
                if (this.exist[index].length > 0) {
                    exist_is_empty = false
                }
            }
            if (!exist_is_empty) {
                words = this.get_shortest_all_places()
            } else {
                words = WORDS
            }
        }
        return words
    }


    // for guesses that contain letters in correct places
    get_shortest_words() {
        let minLength = 5000
        for (let index = 0; index < 5; index++) {
            if (this.correct[index] != "") {
                let curLetterFrequency = (word_dicts[index][this.correct[index]]).length
                if (curLetterFrequency < minLength) {
                    minLength = curLetterFrequency
                    this.min_index = index
                }
            }
        }
        if (this.min_index >= 0) {
            let words = word_dicts[this.min_index][this.correct[this.min_index]]
            return words
        } else {
            return null
        }
    }

    // for guesses that only contains letters in incorrect places
    get_shortest_all_places() {
        let minLength = 5000
        let min_exist_index = -1
        for (let index = 0; index < 5; index++) {
            for (let exist_index = 0; exist_index < this.exist[index].length; exist_index++) {
                if (this.exist[index][exist_index] != "") {
                    let curLetterFrequency = letter_dict[this.exist[index][exist_index]].length
                    if (curLetterFrequency < minLength) {
                        minLength = curLetterFrequency
                        this.min_index = index
                        min_exist_index = exist_index
                    }
                }
            }
        }
        let words = letter_dict[this.exist[this.min_index][min_exist_index]]
        return words
    }

    filterWords(curWords) {
        let correct_is_empty = true
        for (let index = 0; index < 5; index++) {
            if (this.correct[index]) {
                correct_is_empty = false
                break
            }
        }
        let correct_filtered_words
        if (correct_is_empty) {
            correct_filtered_words = curWords
        } else {
            correct_filtered_words = []
            for (let word of curWords) {
                let correct_valid = true
                for (let index = 0; index < 5; index++) {
                    if (this.correct[index] != "") {
                        if (this.correct[index] != word[index]) {
                            correct_valid = false
                            break
                        }
                    }
                }
                if (correct_valid) {
                    if (correct_filtered_words.indexOf(word) < 0) {
                        correct_filtered_words.push(word)
                    }
                }
            }
        }
        let exist_filtered_words = []
        for (let word of correct_filtered_words) {
            let existence_valid = true
            for (let index = 0; index < 5; index++) {
                if (!existence_valid) {
                    break
                }
                if (this.exist[index].indexOf(word[index]) >= 0) {
                    existence_valid = false
                    break
                }
                for (let letter of this.exist[index]) {
                    if (word.indexOf(letter) < 0) {
                        existence_valid = false
                        break
                    }
                }
            }
            if (existence_valid && (exist_filtered_words.indexOf(word) < 0)) {
                exist_filtered_words.push(word)
            }
        }
        let completed_filtered_words = []
        for (let word of exist_filtered_words) {
            let word_valid = true
            for (let wrong_letter of this.incorrect) {
                if (word.indexOf(wrong_letter) >= 0) {
                    word_valid = false
                    break
                }
            }
            if (word_valid) {
                if (completed_filtered_words.indexOf(word) < 0) {
                    completed_filtered_words.push(word)
                }
            }
        }
        return completed_filtered_words
    }

    printStatus() {
        let correct_word = ""
        for (let index = 0; index < 5; index++) {
            if (this.correct[index] != "") {
                correct_word += this.correct[index]
            } else {
                correct_word += "?"
            }
        }
        console.log(`Guess# ${this.guesses}`)
        console.log(`"Current known correct word: ${correct_word}`)
        for (let index = 0; index < 5; index++) {
            console.log(`Index: ${index} | Existing letters: ${this.exist[index]}`)
        }
        console.log(`Incorrect letters: ${this.incorrect}`)
        console.log(`Currently, there are ${(this.possible_words[this.guesses - 1]).length} possible words`)
    }

    get_possible_words() {
        if (this.guesses < 1) {
            return false
        } else {
            return this.possible_words[this.guesses - 1]
        }
    }


    static validate_word(word) {
        if ((word).length != 5) {
            return "Please enter a 5-letter word that you guessed."
        }
        word = word.toLowerCase()
        for (let index = 0; index < 5; index++) {
            // if (!(word[index] in letters)) {
            if (letters.indexOf(word[index]) < 0) {
                return "Words can only contain letters A-Z!"
            }
        }
        return "valid"
    }
}

function loop_logic() {
    let solver = new Solver()
    let letterIndex = 1
    let letterArray = []
    let correct_array = [0, 0, 0, 0, 0]
    let exist_array = [0, 0, 0, 0, 0]
    addColorListeners()
    let feedback_button = document.getElementById("feedback")
    let feedback_form = document.getElementById("feedback_form")
    feedback_button.addEventListener("click", function (event) {
        feedback_form.hidden = false
    })
    let close_feedback = document.getElementById("close_feedback")
    close_feedback.addEventListener("click", function (event) {
        feedback_form.hidden = true
    })
    let results = document.getElementById("results")
    let keyboard_button = document.getElementById("keyboard_button")
    let keyboard_ui = document.getElementById("keyboard")
    keyboard_button.addEventListener("click", function(event) {
        if (keyboard_ui.hidden) {
            keyboard_ui.hidden = false
        } else {
            keyboard_ui.hidden = true
        }
    })

    document.addEventListener('keydown', function (event) {
        let keyInputName = event.key
        process_key_input(keyInputName)
    })

    let onscreen_inputs = []
    for (let letter of letters) {
        onscreen_inputs.push(document.getElementById(`keyboard_${letter}`))
    }
    onscreen_inputs.push(document.getElementById('keyboard_enter'))
    onscreen_inputs.push(document.getElementById('keyboard_delete'))
    onscreen_inputs.forEach(element => element.addEventListener('click', function (event) {
        process_key_input(element.value)
    }) )
    function process_key_input(keyName) {
        if (letters.indexOf(keyName.toLowerCase()) >= 0) {
            if (letterIndex <= 5) {
                let box = document.getElementById(`input${letterIndex}`)
                box.innerHTML = keyName.toUpperCase()
                letterArray.push(keyName.toUpperCase())
                letterIndex++
            }
        } else if (keyName == "Enter") {
            if (letterIndex == 6) {
                console.log("Valid confirm");
                let confirm_input = confirm("Are you sure?")
                console.log(confirm_input)
                if (confirm_input) {
                    let word = letterArray.join("").toLowerCase()
                    console.log(word)
                    let correct_indexes = []
                    let exist_indexes = []
                    for (let i = 0; i < 5; i++) {
                        if (correct_array[i] > 0) {
                            correct_indexes.push(i)
                        }
                        if (exist_array[i] > 0) {
                            exist_indexes.push(i)
                        }
                    }
                    process(word, correct_indexes, exist_indexes)
                    results.innerHTML = ""
                    solver.printStatus()
                    let possible_words = solver.get_possible_words()
                    if (possible_words) {
                        let printed_count = 0
                        let word_block = "<ul>"
                        for (let word of possible_words) {
                            word_block += "<li>" + word + "</li>"
                            printed_count++
                            if (printed_count % 5 == 0) {
                                word_block += "</ul><ul>"
                            }
                            if (printed_count >= 30) {
                                break
                            }
                        }
                        results.innerHTML += word_block
                        if (printed_count >= 30) {
                            setupViewMore(results, possible_words)
                        }
                    } else {
                        results.innerHTML = "<p>" + "I don't know how you got here but you must make a guess first." + "</p>"
                    }
                    results.hidden = false
                    clear_prevIds()
                    render_new_rows()
                    addColorListeners()
                    letterIndex = 1
                    letterArray = []
                    correct_array = [0, 0, 0, 0, 0]
                    exist_array = [0, 0, 0, 0, 0]
                }
            } else {
                console.log("Warning")
                addError("You don't have 5 letters in your word!")
            }
        } else if (keyName == "Backspace") {
            if (letterIndex > 1) {
                letterIndex--
                let box = document.getElementById(`input${letterIndex}`)
                letterArray.pop()
                box.innerHTML = "-"
            }
        }
    }

    let process_button = document.getElementById("process")
    process_button.addEventListener("click", function (event) {
        if (letterIndex == 6) {
            let word = letterArray.join("").toLowerCase()
            console.log(word)
            let correct_indexes = []
            let exist_indexes = []
            for (let i = 0; i < 5; i++) {
                if (correct_array[i] > 0) {
                    correct_indexes.push(i)
                }
                if (exist_array[i] > 0) {
                    exist_indexes.push(i)
                }
            }
            process(word, correct_indexes, exist_indexes)
            results.innerHTML = ""
            solver.printStatus()
            let possible_words = solver.get_possible_words()
            if (possible_words) {
                let printed_count = 0
                let word_block = "<ul>"
                for (let word of possible_words) {
                    word_block += "<li>" + word + "</li>"
                    printed_count++
                    if (printed_count % 5 == 0) {
                        word_block += "</ul><ul>"
                    }
                    if (printed_count >= 30) {
                        break
                    }
                }
                results.innerHTML += word_block
                if (printed_count >= 30) {
                    setupViewMore(results, possible_words)
                }
            } else {
                results.innerHTML = "<p>" + "I don't know how you got here but you must make a guess first." + "</p>"
            }
            results.hidden = false
            clear_prevIds()
            render_new_rows()
            addColorListeners()
            letterIndex = 1
            letterArray = []
            correct_array = [0, 0, 0, 0, 0]
            exist_array = [0, 0, 0, 0, 0]
        } else {
            console.log("Warning")
            addError("You don't have 5 letters in your word!")
        }
    })

    function addColorListeners() {
        let boxes = []
        let buttons = []
        for (let i = 1; i <= 5; i++) {
            buttons.push([])
            boxes.push(document.getElementById(`box${i}`))
            buttons[i - 1].push(document.getElementById(`green${i}`))
            buttons[i - 1].push(document.getElementById(`yellow${i}`))
            buttons[i - 1].push(document.getElementById(`clear${i}`))
            buttons[i - 1][0].addEventListener('click', function (event) {
                buttons[i - 1][0].disabled = true
                buttons[i - 1][1].disabled = false
                buttons[i - 1][2].disabled = false
                boxes[i - 1].classList.remove("bg-light", "bg-success", "bg-warning")
                boxes[i - 1].classList.add("bg-success")
                correct_array[i - 1] = 1
                exist_array[i - 1] = 0
            })
            buttons[i - 1][1].addEventListener('click', function (event) {
                buttons[i - 1][0].disabled = false
                buttons[i - 1][1].disabled = true
                buttons[i - 1][2].disabled = false
                boxes[i - 1].classList.remove("bg-light", "bg-success", "bg-warning")
                boxes[i - 1].classList.add("bg-warning")
                correct_array[i - 1] = 0
                exist_array[i - 1] = 1
            })
            buttons[i - 1][2].addEventListener('click', function (event) {
                buttons[i - 1][0].disabled = false
                buttons[i - 1][1].disabled = false
                buttons[i - 1][2].disabled = true
                boxes[i - 1].classList.remove("bg-light", "bg-success", "bg-warning")
                boxes[i - 1].classList.add("bg-light")
                correct_array[i - 1] = 0
                exist_array[i - 1] = 0
            })
        }
    }

    function clear_prevIds() {
        for (let i = 1; i <= 5; i++) {
            document.getElementById(`colors${i}`).remove()
            document.getElementById(`input${i}`).removeAttribute('id')
            document.getElementById(`box${i}`).removeAttribute('id')
        }
    }

    function render_new_rows() {
        let rows = document.getElementById("rows")
        rows.innerHTML +=
            `<div class="row g-3">
                <div class="col word_box">
                    <div class="bg-light word" id="box1">
                        <p id="input1">-</p>
                    </div>
                    <div id="colors1" class="colors" >
                        <div>
                            <button class="btn btn-success"  id="green1">Green</button>
                            <button class="btn btn-warning" id="yellow1">Yellow</button>
                        </div>
                        <button class="btn btn-light border" id="clear1" disabled>Clear</button>
                    </div>
                </div>
                <div class="col word_box">
                    <div class="bg-light word" id="box2">
                        <p id="input2">-</p>
                    </div>
                    <div id="colors2" class="colors">
                        <div>
                            <button class="btn btn-success"  id="green2">Green</button>
                            <button class="btn btn-warning" id="yellow2">Yellow</button>
                        </div>
                        <button class="btn btn-light border" id="clear2" disabled>Clear</button>
                    </div>
                </div>
                <div class="col word_box">
                    <div class="bg-light word" id="box3">
                        <p id="input3">-</p>
                    </div>
                    <div id="colors3" class="colors">
                        <div>
                            <button class="btn btn-success"  id="green3">Green</button>
                            <button class="btn btn-warning" id="yellow3">Yellow</button>
                        </div>
                        <button class="btn btn-light border" id="clear3" disabled>Clear</button>
                    </div>
                </div>
                <div class="col word_box">
                    <div class="bg-light word" id="box4">
                        <p id="input4">-</p>
                    </div>
                    <div id="colors4" class="colors">
                        <div>
                            <button class="btn btn-success"  id="green4">Green</button>
                            <button class="btn btn-warning" id="yellow4">Yellow</button>
                        </div>
                        <button class="btn btn-light border" id="clear4" disabled>Clear</button>
                    </div>
                </div>
                <div class="col word_box">
                    <div class="bg-light word" id="box5">
                        <p id="input5">-</p>
                    </div>
                    <div id="colors5" class="colors">
                        <div>
                            <button class="btn btn-success"  id="green5">Green</button>
                            <button class="btn btn-warning" id="yellow5">Yellow</button>
                        </div>
                        <button class="btn btn-light border" id="clear5" disabled>Clear</button>
                    </div>
                </div>
            </div>`
    }

    function process(curWord, correct, exist) {
        solver.process_word(curWord, correct, exist)
        solver.process_possible_words()
    }

    function setupViewMore(results, possible_words) {
        results.innerHTML += '<button class="btn btn-light border" id="view_more">View More</button>'
        let view_more = document.getElementById("view_more")
        let more_words = document.getElementById("more_words")
        console.log(view_more)
        view_more.addEventListener("click", function (event) {
            let more_text_block = '<button class="btn btn-warning" id="close_more">Close</button>'
            more_text_block += "<ul>"

            for (let word of possible_words) {
                more_text_block += "<li>" + word + "</li>"
            }
            more_text_block += "</ul>"
            more_words.innerHTML = more_text_block
            more_words.hidden = false
            let close_more = document.getElementById("close_more")
            close_more.addEventListener("click", function (event) {
                more_words.hidden = true
            })
        })
    }

    function addError(message) {
        let warning = document.getElementById("warning")
        warning.innerHTML = "<p class='alert alert-danger alert-dismissible fade show' role='alert'>" + message +
            '<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>' + "</p>"
    }
}


window.addEventListener("load", function () {
    loop_logic()
});