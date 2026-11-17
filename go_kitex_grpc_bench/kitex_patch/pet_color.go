// Kitex v0.16.4 does not emit enums nested two levels deep (Hello.Pet.Color),
// so this file is copied into the generated package at image build time.
package helloworld

type Hello_Pet_Color int32

const (
	Hello_Pet_BLACK  Hello_Pet_Color = 0
	Hello_Pet_WHITE  Hello_Pet_Color = 1
	Hello_Pet_BLUE   Hello_Pet_Color = 2
	Hello_Pet_RED    Hello_Pet_Color = 3
	Hello_Pet_YELLOW Hello_Pet_Color = 4
	Hello_Pet_GREEN  Hello_Pet_Color = 5
)
