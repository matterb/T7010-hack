\ double linked list entry
: dummy ;

begin-structure /dlentry
    field:  >dl-data
    field:  >dl-prev
    field:  >dl-next
end-structure


begin-structure /slist
    field:  >sl-head        \ dlentry
    field:  >sl-tail        \ dlentry
end-structure


: list-init ( list --)   \ initialize list
    0 tuck over >sl-head ! >sl-tail !
;

: list-create ( -- slist) \ create a singly linked list
    /slist allocate throw ( slist )
    dup list-init
;

: list-destruct ( slist ) \ 
    >sl-head @ begin dup while ( dlentry )
        >dl-next @ dup free throw
    repeat
    drop
;

: list-free ( list -- ) dup list-destruct free throw ;

\ check for empy list
: list-empty? ( slist -- flg ) >sl-head @ 0= ;

: list-count ( slist -- +n )
    1 swap >sl-head @ begin        ( count list )
        dup while
        >dl-next @ swap 1+ swap
    repeat
    drop
;


: list-append ( buf list -- dlentry )
    /dlentry allocate throw
    rot  over >dl-data !        ( list dlentry)
    0 over >dl-next !
    over >sl-tail @ over >dl-prev !
    over >sl-head @ if
        2dup swap >sl-tail @
        >dl-next !
    else
        2dup swap >sl-head !
    then
    dup rot >sl-tail !
;

\ uses sentinel  fails test 
: list-element-get ( num slist -- dlentry)
    >sl-head @ begin dup and while  ( number dlentry)
        >dl-next @ swap 1- swap
    repeat
    nip
;


: list-find ( buf slist -- dlentry )
    >sl-head @ begin       ( buf dlentry )
    2dup >dl-data @ <> over and while
        >dl-next
    repeat
    nip
;

0 [if]
dlelement_t* list_insert(list_t* list, dlelement_t* next, void* data)
{
    if (next == 0)
    {
        return (list_append(list, data));
    }

    dlelement_t* newElement = malloc(sizeof(dlelement_t), 0, "listElement");
    if (newElement)
    {
        newElement->data = data;

        if (next == list->head)
        {
            newElement->next = list->head;
            newElement->prev = 0;
            list->head->prev = newElement;
            list->head       = newElement;
        }
        else
        {
            newElement->prev = next->prev;
            newElement->next = next;
            next->prev->next = newElement;
            next->prev       = newElement;
        }

        return newElement;
    }

    return (0);
}

dlelement_t* list_delete(list_t* list, dlelement_t* elem)
{
    if (list->head == 0)
    {
        return (0);
    }

    if (list->head == list->tail)
    {
        free(elem);
        list->head = list->tail = 0;
        return (0);
    }

    dlelement_t* temp = elem->next;

    if (elem == list->head)
    {
        list->head       = elem->next;
        list->head->prev = 0;
    }
    else if (elem == list->tail)
    {
        list->tail       = elem->prev;
        list->tail->next = 0;
    }
    else
    {
        elem->prev->next = elem->next;
        elem->next->prev = elem->prev;
    }

    free(elem);

    return temp;
}

[then]

