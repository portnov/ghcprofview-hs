ghcprofview README
==================

This is GHC `.prof` files viewer, implemented in Haskell + Gtk3.

Unlike [profiterole][1] and [profiteur][2], `ghcprofview` uses a traditional
approach to profiling. It allows you to view cost centres tree as it is and
browse it interactively, and allows you to do some actions that you may be used
to in, for example, Java's `visualvm`.

See also a very similar application in Python + Qt5 - [ghcprofview-py][3].

[1]: https://hackage.haskell.org/package/profiterole
[2]: https://hackage.haskell.org/package/profiteur
[3]: https://github.com/portnov/ghcprofview-py

![Screenshot](https://user-images.githubusercontent.com/284644/61590344-1a57ff80-abd1-11e9-93ce-dfc316c825ae.png)

Features
--------

* GUI is tab-oriented. Default tab is called "All" and contains the whole tree.
  Other tabs may appear when you do filtering or some other actions. You may
  close unneeded tabs.
* Two additional columns in addition to what we have in standard GHC's text `.prof` output:
  * Time Relative: share of "Time Inherited" of this item with relation to it's
    parent item. For example, if this item has "Time Inherited" 20%, and it's
    parent has "Time Inherited" 30%, then "Time Relative" is 20% / 30% =
    66.66%.
  * Alloc Relative: same, but about "Alloc Inherited".
* Click on column header to sort by that column.
* Right-click on table header to select which columns to display.
* Double-click at the edge of column header to adjust column width automatically.
* Use Search and Next buttons to search function by name. There are three
  search modes available: Contains (search by substring), Exact (search for
  exact match), Reg.Exp (search by regular expression).
* Use filters to display interesting records only. Filter results will be shown
  in separate tab.
  * Supported fields for filtering are: Entries, Time Individual, Alloc
    Individual, Time Inherited, Alloc Inherited, Module (by substring match),
    Source (by substring match).
  * Filter works by AND; so if you set Entries = 5, Module = "Gui", then you
    will be searching for items that have entries >= 5 AND in module "Gui".
  * Logic of filter application to the tree is the following: it keeps an item
    if that item conforms to filter conditions, OR if it has child items that
    conform to filter condition.
* "Narrow view to selected item" in right-click menu. This will open a tab and
  show only selected item and it's descendants.
* "Group all outgoing calls" in right-click menu. This does the following:
  * Searches for all occurences of selected function in the tree.
  * Merges call subtrees of these occurences into new tree; for example, if
    function "search" appeared in one place with "time inherited" of 15%, and
    in another place with 10%, then in the merged tree you will see it with
    25%.
  * Displays the result in a new tab.
* "Group all incoming calls" in right-click menu. This does the following:
  * Searches for all occurences of selected function in the tree.
  * Reverses call stacks of found occurences and merges them into a new tree.
    So in that tree, the root will be the item you selected, and it's children
    will be all functions that call the selected function, and so on. Numbers
    are merged similar to "group all outgoing calls" function.
  * Displays the result in a new tab.
* Text format of `.prof` files is supported; there is support for Json format,
  but it is buggy currently.

Installation
------------

Install it by `stack`:

    $ git clone https://github.com/portnov/ghcprofview-hs.git
    $ cd ghcprofview-hs/
    $ stack install

