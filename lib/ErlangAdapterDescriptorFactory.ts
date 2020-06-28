import { 
    DebugAdapterDescriptorFactory, DebugSession, DebugAdapterExecutable, ProviderResult, DebugAdapterDescriptor,
    DebugAdapterServer, DebugAdapterInlineImplementation
} from 'vscode'; 

import { AddressInfo, Server, createServer } from 'net';
import {ErlangDebugSession} from './erlangDebugSession';

export class ErlangDebugAdapterDescriptorFactory implements DebugAdapterDescriptorFactory {
    private server?: Server;
    createDebugAdapterDescriptor(session: DebugSession, executable: DebugAdapterExecutable): ProviderResult<DebugAdapterDescriptor> {
        if (!this.server) {
			// start listening on a random port
			this.server = createServer(socket => {
				const session = new ErlangDebugSession(true);
				session.setRunAsServer(true);
				session.start(<NodeJS.ReadableStream>socket, socket);
			}).listen(0);
		}

		// make VS Code connect to debug server
		return new DebugAdapterServer((<AddressInfo>this.server.address()).port);
    }

    dispose() {
		if (this.server) {
			this.server.close();
        }
    }
}

export class InlineErlangDebugAdapterFactory implements DebugAdapterDescriptorFactory {
    createDebugAdapterDescriptor(session: DebugSession, executable: DebugAdapterExecutable): ProviderResult<DebugAdapterDescriptor> {
        return new DebugAdapterInlineImplementation(new ErlangDebugSession(true));
    }
}

export class ErlangDebugAdapterExecutableFactory implements DebugAdapterDescriptorFactory {
	/**
	 *
	 */
	private  extenstionPath : string;
	constructor(extenstionPath : string ) {
		this.extenstionPath = extenstionPath;
	}

    createDebugAdapterDescriptor(session: DebugSession, executable: DebugAdapterExecutable): ProviderResult<DebugAdapterDescriptor> {
		// param "executable" contains the executable optionally specified in the package.json (if any)

		// use the executable specified in the package.json if it exists or determine it based on some other information (e.g. the session)
		if (!executable) {
			const command = "node";
			const args = [
				"./out/lib/erlangDebug.js",
			];
			const options = {
				cwd: this.extenstionPath,
				//env: { "VAR": "some value" }
			};
			executable = new DebugAdapterExecutable(command, args, options);
		}

		// make VS Code launch the DA executable
		return executable;
    }
}
