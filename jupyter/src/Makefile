PIPENV_RUN=pipenv run

reinstall:
	${PIPENV_RUN} jupyter kernelspec uninstall -f askee_kernel || true
	pipenv install
	${PIPENV_RUN} jupyter kernelspec install askee_kernel --user

.PHONY: lab
lab:
	${PIPENV_RUN} jupyter lab

.PHONY: reinstall-lab
reinstall-lab: reinstall lab
