#include <stdlib.h>
#include <string.h>
#include <ctype.h>
 
typedef struct item_t item_t, *item;
struct item_t { const char *name; int *deps, n_deps, idx, depth; };
 
int get_item(item *list, int *len, const char *name)
{
	int i;
	item lst = *list;
 
	for (i = 0; i < *len; i++)
		if (!strcmp(lst[i].name, name)) return i;
 
	lst = *list = realloc(lst, ++*len * sizeof(item_t));
	i = *len - 1;
	memset(lst + i, 0, sizeof(item_t));
	lst[i].idx = i;
	lst[i].name = name;
	return i;
}
 
void add_dep(item it, int i)
{
	if (it->idx == i) return;
	it->deps = realloc(it->deps, (it->n_deps + 1) * sizeof(int));
	it->deps[it->n_deps++] = i;
}
 
int get_depth(item list, int idx, int bad)
{
	int max, i, t;
 
	if (!list[idx].deps)
		return list[idx].depth = 1;
 
	if ((t = list[idx].depth) < 0) return t;
 
	list[idx].depth = bad;
	for (max = i = 0; i < list[idx].n_deps; i++) {
		if ((t = get_depth(list, list[idx].deps[i], bad)) < 0) {
			max = t;
			break;
		}
		if (max < t + 1) max = t + 1;
	}
	return list[idx].depth = max;
}

int toposort(item items)
{
	int i, j, n, bad = -1, max, min;
 
	for (i = 0; i < n; i++)
		if (!items[i].depth && get_depth(items, i, bad) < 0) bad--;
 
	for (i = 0, max = min = 0; i < n; i++) {
		if (items[i].depth > max) max = items[i].depth;
		if (items[i].depth < min) min = items[i].depth;
	}
 
	return 0;
}
