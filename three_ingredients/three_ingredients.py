#!/usr/bin/env python3

"""
Empirical "solution" of the "Incompatible Food Triad" problem:

> Find three ingredients to which pairs of them go well together, but when you mix the three, it tastes horribly


Used https://github.com/Glorf/recipenlg to collect >2M recipes, and find candidate triplets and defining a tf/idf score. 
Unusual standalone ingredients (e.g. oil, etc) were discarded.


Spoiler:

    The winner (at least in my opinion was): Rice, Carrot and Vanilla


"""


from collections import Counter, defaultdict

import csv
import json
from tqdm import tqdm

# singles = Counter()
pairs = Counter()
pair_seq = defaultdict(set)
triplets = Counter()

stop_ingr = {
    "salt",
    "sugar",
    "mix",
    "all-purpose",
    "water",
    "boiling water",
    "baking powder",
    "shortening",
    "allspice",
    "cooking oil",
    "pecans",
    "Wesson oil",
    "baking soda",
    "cornstash",
    "oleo",
    "vinegar",
    "brown sugar",
    "white sugar",
    "flour",
    "olive oil",
    "vanilla",
    "soda",
    "¼",
    "Salt",
    "chicken broth",
    "oil",
    "margarine",
    "vegetable oil",
    "powdered sugar",
    "cornstarch",
    "yeast",
    "FILLING",
    "Sugar",
    "Baking Powder",
    "CAKE",
    "canola oil",
    "salad oil",
}
with open("dataset/full_dataset.csv", "r") as f:
    reader = csv.reader(f)
    next(reader)  # skip header

    x = []
    INGREDIENTS_COLUMN = -1
    for i, row in tqdm(enumerate(reader)):
        ingr = json.loads(row[INGREDIENTS_COLUMN])
        ingr = sorted(list(set(ingr)))  # remove duplicates and sort
        ingr = list(filter(lambda i: i not in stop_ingr, ingr))  # remove stop ingr
        for a, ing in enumerate(ingr):
            # singles.update((ing,))
            pair_seq[ingr[a]].update(ingr[a + 1 :])
            for b in range(a + 1, len(ingr)):
                pairs.update(((ingr[a], ingr[b]),))
                # pair_seq[ingr[b]].update(ingr[b + 1 :])
                for c in range(b + 1, len(ingr)):
                    triplets.update(((ingr[a], ingr[b], ingr[c]),))

        if i == 10_000_000:
            break


# print(singles)
# print(pairs)
# print(triplets)
print("done collecting, now finding results")

best_scores = []
worst_score = 0
for a, b in tqdm(pairs):
    for c in pair_seq[a] & pair_seq[b]:
        aa, bb, cc = sorted([a, b, c])
        if (aa, bb, cc) in triplets:
            continue
        score = min(pairs[(aa, bb)], pairs[(bb, cc)], pairs[(aa, cc)])
        if score > worst_score:
            best_scores.append((score, (aa, bb, cc)))
            best_scores.sort(reverse=True)
            best_scores = best_scores[:100]
            worst_score = best_scores[-1][0]

# print(sorted(scores.items(), key=lambda kv: kv[1], reverse=True)[:100])
print(best_scores)
