package main

import (
	"fmt"
	"sync"
	"time"
)

var wg sync.WaitGroup

type Promise struct {
	ch    chan struct{}
	value int
}

func (p *Promise) Resolve(value int) {
	p.value = value
	close(p.ch)
}

func (p *Promise) Await() int {
	<-p.ch
	return p.value
}

func listen(p *Promise, id int) {
	wg.Add(1)
	go func() {
		defer wg.Done()
		got := p.Await()
		fmt.Printf("%d got %d \n", id, got)
	}()
}

func main() {
	promise := &Promise{
		ch:    make(chan struct{}),
		value: -1,
	}
	// Establish two listeners. Let's see who get the value
	listen(promise, 0)
	listen(promise, 1)
	go func() {
		time.Sleep(time.Second)
		promise.Resolve(0)
	}()

	wg.Wait()
}
