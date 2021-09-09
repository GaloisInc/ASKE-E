FROM python:3.8

RUN pip install jupyter pipenv

COPY Pipfile Pipfile.lock pyproject.toml setup.cfg /build/
COPY src /build/src
WORKDIR /build/src
RUN make reinstall

COPY demo-notebooks /build/src/demo-notebooks

EXPOSE 8888
ENTRYPOINT pipenv run jupyter lab --allow-root --no-browser --ip=0.0.0.0 --port=8888
