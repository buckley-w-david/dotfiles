package main

import "fmt"
import "strings"
import "path/filepath"
import "os"

// https://stackoverflow.com/a/53737602/14470574
func prependString(x []string, y string) []string {
	x = append(x, "")
	copy(x[1:], x)
	x[0] = y
	return x
}

func main() {
	userHome, err := os.UserHomeDir()
	if err != nil {
		panic("Halp")
	}
	userPath := strings.Replace(os.Args[1], userHome, "~", 1)

	part := filepath.Base(userPath)
	parts := make([]string, strings.Count(userPath, string(filepath.Separator)))

	parts = append(parts, part)
	userPath = filepath.Dir(userPath)
	for !(userPath == "/" || userPath == "~" || userPath == ".") {
		part := filepath.Base(userPath)
		parts = prependString(parts, part[0:1])
		userPath = filepath.Dir(userPath)
	}
	parts = prependString(parts, filepath.Base(userPath))
	fmt.Println(filepath.Join(parts...))
}
