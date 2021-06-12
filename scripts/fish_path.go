package main

import "fmt"
import "strings"
import "path/filepath"
import "os"

func reverse(ss []string) {
	last := len(ss) - 1
	for i := 0; i < len(ss)/2; i++ {
		ss[i], ss[last-i] = ss[last-i], ss[i]
	}
}

func main() {
	var parts []string

	userHome, err := os.UserHomeDir()
	if err != nil {
		panic("Halp")
	}
	userPath := strings.Replace(os.Args[1], userHome, "~", 1)

	part := filepath.Base(userPath)
	parts = append(parts, part)
	userPath = filepath.Dir(userPath)
	for !(userPath == "/" || userPath == "~") {
		part := filepath.Base(userPath)
		parts = append(parts, part[0:1])
		userPath = filepath.Dir(userPath)
	}
	parts = append(parts, filepath.Base(userPath))
	reverse(parts)
	fmt.Println(filepath.Join(parts...))
}
