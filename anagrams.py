#!/usr/bin/python

from unidecode import unidecode
from itertools import combinations


def fit(word, letters):
    for w in word:
        if w not in letters:
            return ""
        elif w in letters:
            letters = letters.replace(w, "", 1)

    return letters


def finish_him(remaining, w_dict):
    w_dict_set = set(w_dict)
    return any_words(remaining, w_dict_set)


def any_words(remaining, w_dict):
    results = []
    for n in range(len(remaining)):
        for w in combinations(remaining, n+1):
            if "".join(w) in w_dict:
                rem_remaining = remaining
                for char in w:
                    rem_remaining = rem_remaining.replace(char, "", 1)
                for r in any_words(rem_remaining, w_dict):
                    result = ["".join(w)]
                    if r:
                        result.extend(r)
                    results.append(result)

    if not results:
        results = [remaining]

    return results


def normalize(w):
    return unidecode(w.lower().strip())


def clean_and_extend_candidates(candidates, completed,
                                to_be_appended, to_be_removed):
    candidates.extend(to_be_appended)
    for candidate in candidates:
        if candidate["letters"] == "":
            completed.append(candidate)
            print(completed[-1]["formed_words"])
            to_be_removed.append(candidate)
    for rem in to_be_removed:
        candidates.remove(rem)
    return candidates, completed


def main():

    init_letters = normalize("felizaniversário")
    initial_candidate = {
        "letters": init_letters,
        "formed_words": [],
        }
    # TODO: read from imput

    to_be_appended = [initial_candidate]
    candidates = []
    completed = []
    to_be_removed = []

    with open("/usr/share/dict/pt-br") as f:
        word_dict = [normalize(w) for w in f.readlines() if len(normalize(w)) > 2]

    for i, w in enumerate(word_dict):
        print(w)
        candidates, completed = clean_and_extend_candidates(candidates,
                                                            completed,
                                                            to_be_appended,
                                                            to_be_removed)
        to_be_appended = []
        to_be_removed = []
        for candidate in candidates:
            r = fit(w, candidate["letters"])
            if r:
                print("found", w, len(candidates))
                # import ipdb; ipdb.set_trace()
                new_words = candidate["formed_words"][:]
                new_words.append(word_dict[i])  # why can't I do this in one line?
                if len(r) > 5:
                    new_candidate = {
                        "letters": r,
                        "formed_words": new_words
                    }
                    to_be_appended.append(new_candidate)
                else:
                    for end in finish_him(r, word_dict[i+1:]):
                        new_new_words = new_words[:]
                        new_new_words.extend(end)
                        new_candidate = {
                            "letters": "",
                            "formed_words": new_new_words
                        }
                        completed.append(new_candidate)


    print("Palavras encontradas:")
    print("Completas:")
    for i, candidate in enumerate(completed):
        print(i+1)
        print(" ".join(candidate["formed_words"]))
        print(candidate["letters"])
    print("Not complete")
    for i, candidate in enumerate(candidates):
        print(i+1)
        print(" ".join(candidate["formed_words"]))
        print(candidate["letters"])


if __name__ == '__main__':
    main()
