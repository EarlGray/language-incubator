#ifndef __STD_CONTAINERS_H
#define __STD_CONTAINERS_H

/// -----------------------------------------------------------------
template <typename T>
class Stack 
{
public:
    struct Node {
        T data;
        Node *prev;
    };
protected:
    Node *top;
    
public:
    Stack():
        top(NULL)
        {}

    virtual ~Stack() {
        while (top != NULL) 
            pop();
    }

    void push(T x) {
        Node *t = new Node;
        t->prev = top;
        top = t;
        top->data = x;
    }

    T pop() {
        if (top == NULL) {
            cerr << "No items to pop from stack\n";
            return (T)0;
        }
        T t = top->data;
        Node *tnode = top;
        top = top->prev;
        delete tnode;
        return t;
    }


};

template <typename T> 
class List
{
public:
    struct Node {
        T data;
        Node *next;
    };

protected:
    Node *first;
    Node *last;

    void delete_tail(Node *from) {
        if (from->next != NULL)
        {
            delete_tail(from->next);
            delete from->next;
        }
    }

public:
    void add(T x) {
        if (first == NULL) {
            first = new Node;
            last = first;
        } else {
            last->next = new Node;
            last = last->next;
        }

        last->data = x;
        last->next = NULL;
    }

    unsigned length() {
        Node *current = first;
        unsigned res = 0;
        while (current != NULL) {
            current = current->next;
            ++res;
        }
        return res;
    }

    T* item(unsigned index) {
        Node *current = first;
        for (int i = 0; i < index; ++i)
            if (current != NULL)
                current = current->next;
            else {
                cerr << "List: item[" << index << "] is out of bounds\n";
                return (T *)NULL;
            }
        return &(current->data);
    }

    Node* getFirst() {
        return first;
    }

    List():
        first(NULL),
        last(NULL)
    {
    }

    virtual ~List() {
        delete_tail(first);
        delete first;
    }
};

#endif //__STD_CONTAINERS_H
