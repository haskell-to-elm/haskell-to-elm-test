.PHONY: run-backend
run-backend:
	cd backend \
	  && stack build --fast :backend-exe \
	  && stack exec -- backend-exe

.PHONY: run-frontend
run-frontend: generate-client-library frontend/node_modules
	cd frontend \
	  && yarn run --silent elm-app start


.PHONY: generate-client-library
generate-client-library:
	cd backend \
	  && stack build --fast :elm-generator \
	  && stack exec -- elm-generator ../frontend/generated_src/

frontend/node_modules: frontend/package.json frontend/yarn.lock
	cd frontend \
	  && yarn install
