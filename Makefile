# debug
FPC_OPTIONS:=-Sc -Sa -g -gl -gh
# release
#FPC_OPTIONS:=-Sc

.PHONY: all
all: pascal-lint tests/test_passrc_two_times

%: %.lpr
	fpc $(FPC_OPTIONS) $<

.PHONY: clean
clean:
	rm -f pascal-lint \
	      pascal-lint.exe \
	      tests/test_passrc_two_times \
	      tests/test_passrc_two_times.exe
